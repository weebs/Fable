from abc import abstractmethod
from collections import deque
from threading import Timer, Lock, RLock
from fable.util import IDisposable


class OperationCanceledError(Exception):
    def __init__(self):
        super().__init__("The operation was canceled")


class CancellationToken:
    def __init__(self, cancelled: bool = False):
        self.cancelled = cancelled
        self.listeners = {}
        self.idx = 0
        self.lock = RLock()

    @property
    def is_cancelled(self):
        return self.cancelled

    def cancel(self):
        # print("CancellationToken:cancel")
        cancel = False
        with self.lock:
            if not self.cancelled:
                cancel = True
                self.cancelled = True

        # print("cancel", cancel)
        if cancel:
            for listener in self.listeners.values():
                listener()

    def add_listener(self, f):
        with self.lock:
            id = self.idx
            self.idx = self.idx + 1
            self.listeners[self.idx] = f

        return id

    def remove_listener(self, id: int):
        with self.lock:
            del self.listeners[id]

    def register(self, f, state=None):
        if state:
            id = self.add_listener(lambda _=None: f(state))
        else:
            id = self.add_listener(f)

        def dispose():
            self.remove_listener(id)

        IDisposable.create(dispose)


class IAsyncContext:
    @abstractmethod
    def on_success(self, value=None):
        ...

    @abstractmethod
    def on_error(self, error):
        ...

    @abstractmethod
    def on_cancel(self, ct):
        ...

    @property
    @abstractmethod
    def trampoline(self) -> "Trampoline":
        ...

    @property
    @abstractmethod
    def cancel_token(self) -> CancellationToken:
        ...

    @staticmethod
    def create(on_success, on_error, on_cancel, trampoline, cancel_token):
        return AnonymousAsyncContext(on_success, on_error, on_cancel, trampoline, cancel_token)


class AnonymousAsyncContext:
    def __init__(self, on_success=None, on_error=None, on_cancel=None, trampoline=None, cancel_token=None):
        self._on_success = on_success
        self._on_error = on_error
        self._on_cancel = on_cancel

        self.cancel_token = cancel_token
        self.trampoline = trampoline

    def on_success(self, value=None):
        return self._on_success(value)

    def on_error(self, error):
        return self._on_error(error)

    def on_cancel(self, ct):
        return self._on_cancel(ct)


# type IAsync<'T> = IAsyncContext<'T> -> unit


class Trampoline:
    MaxTrampolineCallCount = 900  # Max recursion depth: 1000

    def __init__(self):
        self.call_count = 0
        self.lock = Lock()
        self.queue = deque()
        self.running = False

    def increment_and_check(self):
        with self.lock:
            self.call_count = self.call_count + 1
            return self.call_count > Trampoline.MaxTrampolineCallCount

    def run(self, action):

        if self.increment_and_check():
            with self.lock:
                # print("queueing...")
                self.queue.append(action)

            if not self.running:
                self.running = True
                timer = Timer(0.0, self._run)
                timer.start()
        else:
            action()

    def _run(self):
        while len(self.queue):
            with self.lock:
                self.call_count = 0
                action = self.queue.popleft()
                # print("Running action: ", action)

            action()

        self.running = False


def protected_cont(f):
    # print("protected_cont")

    def _protected_cont(ctx):
        # print("_protected_cont")

        if ctx.cancel_token.is_cancelled:
            ctx.on_cancel(OperationCanceledError())

        def fn():
            try:
                return f(ctx)
            except Exception as err:
                print("Exception: ", err)
                ctx.on_error(err)

        ctx.trampoline.run(fn)

    return _protected_cont


def protected_bind(computation, binder):
    # print("protected_bind")

    def cont(ctx):
        # print("protected_bind: inner")

        def on_success(x):
            # print("protected_bind: x", x, binder)
            try:
                binder(x)(ctx)
            except Exception as err:
                print("Exception: ", err)
                ctx.on_error(err)

        ctx_ = IAsyncContext.create(on_success, ctx.on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token)
        return computation(ctx_)

    return protected_cont(cont)


def protected_return(value=None):
    # print("protected_return:", value)
    return protected_cont(lambda ctx: ctx.on_success(value))


class AsyncBuilder:
    def Bind(self, computation, binder):
        return protected_bind(computation, binder)

    def Combine(self, computation1, computation2):
        return self.Bind(computation1, lambda _=None: computation2)

    def Delay(self, generator):
        # print("Delay", generator)
        return protected_cont(lambda ctx: generator()(ctx))

    def For(self, sequence, body):
        print("For", sequence)

        done = False
        it = iter(sequence)
        try:
            cur = next(it)
        except StopIteration:
            done = True

        def delay():
            # print("For:delay")
            nonlocal cur, done
            # print("cur", cur)
            res = body(cur)
            # print("For:delay:res", res)
            try:
                cur = next(it)
                # print("For:delay:next", cur)
            except StopIteration:
                print("For:delay:stopIteration")
                done = True
            return res

        return self.While(lambda: not done, self.Delay(delay))

    def Return(self, value=None):
        # print("Return: ", value)
        return protected_return(value)

    def ReturnFrom(self, computation):
        return computation

    def TryFinally(self, computation, compensation):
        def cont(ctx):
            def on_success(x):
                compensation()
                ctx.on_success(x)

            def on_error(x):
                compensation()
                ctx.on_error(x)

            def on_cancel(x):
                compensation()
                ctx.on_cancel(x)

            ctx_ = IAsyncContext.create(on_success, on_error, on_cancel, ctx.trampoline, ctx.cancel_token)
            computation(ctx_)

        return protected_cont(cont)

    def TryWith(self, computation, catchHandler):
        # print("TryWith")

        def fn(ctx):
            def on_error(err):
                try:
                    catchHandler(err)(ctx)
                except Exception as ex2:
                    ctx.on_error(ex2)

            ctx = IAsyncContext.create(
                on_success=ctx.on_success,
                on_cancel=ctx.on_cancel,
                cancel_token=ctx.cancel_token,
                trampoline=ctx.trampoline,
                on_error=on_error,
            )

            return computation(ctx)

        return protected_cont(fn)

    def Using(self, resource, binder):
        return self.TryFinally(binder(resource), lambda _=None: resource.Dispose())

    def While(self, guard, computation):
        # print("while")
        if guard():
            # print("gard()")
            return self.Bind(computation, lambda _=None: self.While(guard, computation))
        else:
            # print("While:return")
            return self.Return()

    def Zero(self):
        return protected_cont(lambda ctx: ctx.on_success())


singleton = AsyncBuilder()