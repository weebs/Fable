module rec soundio

open System.Runtime.InteropServices

type SoundIoError =
    | SoundIoErrorNone = 0
    | SoundIoErrorNoMem = 1
    | SoundIoErrorInitAudioBackend = 2
    | SoundIoErrorSystemResources = 3
    | SoundIoErrorOpeningDevice = 4
    | SoundIoErrorNoSuchDevice = 5
    | SoundIoErrorInvalid = 6
    | SoundIoErrorBackendUnavailable = 7
    | SoundIoErrorStreaming = 8
    | SoundIoErrorIncompatibleDevice = 9
    | SoundIoErrorNoSuchClient = 10
    | SoundIoErrorIncompatibleBackend = 11
    | SoundIoErrorBackendDisconnected = 12
    | SoundIoErrorInterrupted = 13
    | SoundIoErrorUnderflow = 14
    | SoundIoErrorEncodingString = 15
type SoundIoFormat =
    | SoundIoFormatInvalid = 0
    | SoundIoFormatS8 = 1
    | SoundIoFormatU8 = 2
    | SoundIoFormatS16LE = 3
    | SoundIoFormatS16BE = 4
    | SoundIoFormatU16LE = 5
    | SoundIoFormatU16BE = 6
    | SoundIoFormatS24LE = 7
    | SoundIoFormatS24BE = 8
    | SoundIoFormatU24LE = 9
    | SoundIoFormatU24BE = 10
    | SoundIoFormatS32LE = 11
    | SoundIoFormatS32BE = 12
    | SoundIoFormatU32LE = 13
    | SoundIoFormatU32BE = 14
    | SoundIoFormatFloat32LE = 15
    | SoundIoFormatFloat32BE = 16
    | SoundIoFormatFloat64LE = 17
    | SoundIoFormatFloat64BE = 18
type SoundIoBackend =
    | SoundIoBackendNone = 0
    | SoundIoBackendJack = 1
    | SoundIoBackendPulseAudio = 2
    | SoundIoBackendAlsa = 3
    | SoundIoBackendCoreAudio = 4
    | SoundIoBackendWasapi = 5
    | SoundIoBackendDummy = 6
type SoundIoDeviceAim =
    | SoundIoDeviceAimInput = 0
    | SoundIoDeviceAimOutput = 1
type SoundIoChannelId =
    | SoundIoChannelIdInvalid = 0
    | SoundIoChannelIdFrontLeft = 1
    | SoundIoChannelIdFrontRight = 2
    | SoundIoChannelIdFrontCenter = 3
    | SoundIoChannelIdLfe = 4
    | SoundIoChannelIdBackLeft = 5
    | SoundIoChannelIdBackRight = 6
    | SoundIoChannelIdFrontLeftCenter = 7
    | SoundIoChannelIdFrontRightCenter = 8
    | SoundIoChannelIdBackCenter = 9
    | SoundIoChannelIdSideLeft = 10
    | SoundIoChannelIdSideRight = 11
    | SoundIoChannelIdTopCenter = 12
    | SoundIoChannelIdTopFrontLeft = 13
    | SoundIoChannelIdTopFrontCenter = 14
    | SoundIoChannelIdTopFrontRight = 15
    | SoundIoChannelIdTopBackLeft = 16
    | SoundIoChannelIdTopBackCenter = 17
    | SoundIoChannelIdTopBackRight = 18
    | SoundIoChannelIdBackLeftCenter = 19
    | SoundIoChannelIdBackRightCenter = 20
    | SoundIoChannelIdFrontLeftWide = 21
    | SoundIoChannelIdFrontRightWide = 22
    | SoundIoChannelIdFrontLeftHigh = 23
    | SoundIoChannelIdFrontCenterHigh = 24
    | SoundIoChannelIdFrontRightHigh = 25
    | SoundIoChannelIdTopFrontLeftCenter = 26
    | SoundIoChannelIdTopFrontRightCenter = 27
    | SoundIoChannelIdTopSideLeft = 28
    | SoundIoChannelIdTopSideRight = 29
    | SoundIoChannelIdLeftLfe = 30
    | SoundIoChannelIdRightLfe = 31
    | SoundIoChannelIdLfe2 = 32
    | SoundIoChannelIdBottomCenter = 33
    | SoundIoChannelIdBottomLeftCenter = 34
    | SoundIoChannelIdBottomRightCenter = 35
    | SoundIoChannelIdMsMid = 36
    | SoundIoChannelIdMsSide = 37
    | SoundIoChannelIdAmbisonicW = 38
    | SoundIoChannelIdAmbisonicX = 39
    | SoundIoChannelIdAmbisonicY = 40
    | SoundIoChannelIdAmbisonicZ = 41
    | SoundIoChannelIdXyX = 42
    | SoundIoChannelIdXyY = 43
    | SoundIoChannelIdHeadphonesLeft = 44
    | SoundIoChannelIdHeadphonesRight = 45
    | SoundIoChannelIdClickTrack = 46
    | SoundIoChannelIdForeignLanguage = 47
    | SoundIoChannelIdHearingImpaired = 48
    | SoundIoChannelIdNarration = 49
    | SoundIoChannelIdHaptic = 50
    | SoundIoChannelIdDialogCentricMix = 51
    | SoundIoChannelIdAux = 52
    | SoundIoChannelIdAux0 = 53
    | SoundIoChannelIdAux1 = 54
    | SoundIoChannelIdAux2 = 55
    | SoundIoChannelIdAux3 = 56
    | SoundIoChannelIdAux4 = 57
    | SoundIoChannelIdAux5 = 58
    | SoundIoChannelIdAux6 = 59
    | SoundIoChannelIdAux7 = 60
    | SoundIoChannelIdAux8 = 61
    | SoundIoChannelIdAux9 = 62
    | SoundIoChannelIdAux10 = 63
    | SoundIoChannelIdAux11 = 64
    | SoundIoChannelIdAux12 = 65
    | SoundIoChannelIdAux13 = 66
    | SoundIoChannelIdAux14 = 67
    | SoundIoChannelIdAux15 = 68
type SoundIoChannelLayoutId =
    | SoundIoChannelLayoutIdMono = 0
    | SoundIoChannelLayoutIdStereo = 1
    | SoundIoChannelLayoutId2Point1 = 2
    | SoundIoChannelLayoutId3Point0 = 3
    | SoundIoChannelLayoutId3Point0Back = 4
    | SoundIoChannelLayoutId3Point1 = 5
    | SoundIoChannelLayoutId4Point0 = 6
    | SoundIoChannelLayoutIdQuad = 7
    | SoundIoChannelLayoutIdQuadSide = 8
    | SoundIoChannelLayoutId4Point1 = 9
    | SoundIoChannelLayoutId5Point0Back = 10
    | SoundIoChannelLayoutId5Point0Side = 11
    | SoundIoChannelLayoutId5Point1 = 12
    | SoundIoChannelLayoutId5Point1Back = 13
    | SoundIoChannelLayoutId6Point0Side = 14
    | SoundIoChannelLayoutId6Point0Front = 15
    | SoundIoChannelLayoutIdHexagonal = 16
    | SoundIoChannelLayoutId6Point1 = 17
    | SoundIoChannelLayoutId6Point1Back = 18
    | SoundIoChannelLayoutId6Point1Front = 19
    | SoundIoChannelLayoutId7Point0 = 20
    | SoundIoChannelLayoutId7Point0Front = 21
    | SoundIoChannelLayoutId7Point1 = 22
    | SoundIoChannelLayoutId7Point1Wide = 23
    | SoundIoChannelLayoutId7Point1WideBack = 24
    | SoundIoChannelLayoutIdOctagonal = 25
type [<Struct>] SoundIoInStream = {
    device: nativeptr<SoundIoDevice>
    format: SoundIoFormat
    sample_rate: int
    layout: SoundIoChannelLayout
    software_latency: double
    userdata: nativeint
    read_callback: nativeint
    overflow_callback: nativeint
    error_callback: nativeint
    name: nativeptr<char>
    non_terminal_hint: bool
    bytes_per_frame: int
    bytes_per_sample: int
    layout_error: int
}
type [<Struct>] SoundIoOutStream = {
    device: nativeptr<SoundIoDevice>
    format: SoundIoFormat
    sample_rate: int
    layout: SoundIoChannelLayout
    software_latency: double
    volume: single
    userdata: nativeint
    write_callback: nativeint
    underflow_callback: nativeint
    error_callback: nativeint
    name: nativeptr<char>
    non_terminal_hint: bool
    bytes_per_frame: int
    bytes_per_sample: int
    layout_error: int
}
type [<Struct>] SoundIoDevice = {
    soundio: nativeptr<SoundIo>
    id: nativeptr<char>
    name: nativeptr<char>
    aim: SoundIoDeviceAim
    layouts: nativeptr<SoundIoChannelLayout>
    layout_count: int
    current_layout: SoundIoChannelLayout
    formats: nativeptr<SoundIoFormat>
    format_count: int
    current_format: SoundIoFormat
    sample_rates: nativeptr<SoundIoSampleRateRange>
    sample_rate_count: int
    sample_rate_current: int
    software_latency_min: double
    software_latency_max: double
    software_latency_current: double
    is_raw: bool
    ref_count: int
    probe_error: int
}
type [<Struct>] SoundIo = {
    userdata: nativeint
    on_devices_change: nativeint
    on_backend_disconnect: nativeint
    on_events_signal: nativeint
    current_backend: SoundIoBackend
    app_name: nativeptr<char>
    emit_rtprio_warning: nativeint
    jack_info_callback: nativeint
    jack_error_callback: nativeint
}
type [<Struct>] SoundIoChannelArea = {
    ptr: nativeptr<char>
    step: int
}
type [<Struct>] SoundIoSampleRateRange = {
    min: int
    max: int
}
type [<Struct>] SoundIoChannelLayout = {
    name: nativeptr<char>
    channel_count: int
    channels: nativeptr<SoundIoChannelId>
}

type SoundIoRingBuffer = struct end

extern SoundIoInStream * soundio_instream_create (SoundIoDevice * device)
extern int soundio_outstream_set_volume (SoundIoOutStream * outstream, double volume)
extern int soundio_outstream_get_latency (SoundIoOutStream * outstream, double * out_latency)
extern int soundio_outstream_pause (SoundIoOutStream * outstream, bool pause)
extern int soundio_outstream_clear_buffer (SoundIoOutStream * outstream)
extern int soundio_outstream_end_write (SoundIoOutStream * outstream)
extern void soundio_outstream_destroy (SoundIoOutStream * outstream)
extern int soundio_outstream_start (SoundIoOutStream * outstream)
extern int soundio_outstream_open (SoundIoOutStream * outstream)
extern SoundIoOutStream * soundio_outstream_create (SoundIoDevice * device)
extern void soundio_instream_destroy (SoundIoInStream * instream)
extern int soundio_device_nearest_sample_rate (SoundIoDevice * device, int sample_rate)
extern int soundio_outstream_begin_write (SoundIoOutStream * outstream, SoundIoChannelArea * * areas, int * frame_count)
extern int soundio_instream_open (SoundIoInStream * instream)
extern int soundio_instream_get_latency (SoundIoInStream * instream, double * out_latency)
extern int soundio_instream_begin_read (SoundIoInStream * instream, SoundIoChannelArea * * areas, int * frame_count)
extern int soundio_instream_end_read (SoundIoInStream * instream)
extern int soundio_instream_pause (SoundIoInStream * instream, bool pause)
extern bool soundio_device_supports_sample_rate (SoundIoDevice * device, int sample_rate)
extern SoundIoRingBuffer * soundio_ring_buffer_create (SoundIo * soundio, int requested_capacity)
extern void soundio_ring_buffer_destroy (SoundIoRingBuffer * ring_buffer)
extern int soundio_ring_buffer_capacity (SoundIoRingBuffer * ring_buffer)
extern char * soundio_ring_buffer_write_ptr (SoundIoRingBuffer * ring_buffer)
extern void soundio_ring_buffer_advance_write_ptr (SoundIoRingBuffer * ring_buffer, int count)
extern char * soundio_ring_buffer_read_ptr (SoundIoRingBuffer * ring_buffer)
extern void soundio_ring_buffer_advance_read_ptr (SoundIoRingBuffer * ring_buffer, int count)
extern int soundio_ring_buffer_fill_count (SoundIoRingBuffer * ring_buffer)
extern int soundio_ring_buffer_free_count (SoundIoRingBuffer * ring_buffer)
extern void soundio_ring_buffer_clear (SoundIoRingBuffer * ring_buffer)
extern int soundio_instream_start (SoundIoInStream * instream)
extern char * soundio_version_string ()
extern bool soundio_device_supports_format (SoundIoDevice * device, SoundIoFormat format)
extern void soundio_device_sort_channel_layouts (SoundIoDevice * device)
extern void soundio_force_device_scan (SoundIo * soundio)
extern void soundio_wakeup (SoundIo * soundio)
extern void soundio_wait_events (SoundIo * soundio)
extern void soundio_flush_events (SoundIo * soundio)
extern bool soundio_have_backend (SoundIoBackend backend)
extern SoundIoBackend soundio_get_backend (SoundIo * soundio, int index)
[<DllImport("yo")>]
extern int soundio_backend_count (SoundIo * soundio)
extern char * soundio_backend_name (SoundIoBackend backend)
extern char * soundio_strerror (int error)
extern void soundio_disconnect (SoundIo * soundio)
extern int soundio_connect_backend (SoundIo * soundio, SoundIoBackend backend)
extern int soundio_connect (SoundIo * soundio)
extern void soundio_destroy (SoundIo * soundio)
extern SoundIo * soundio_create ()
extern int soundio_version_patch ()
extern int soundio_version_minor ()
extern int soundio_version_major ()
extern bool soundio_channel_layout_equal (SoundIoChannelLayout * a, SoundIoChannelLayout * b)
extern char * soundio_get_channel_name (SoundIoChannelId id)
extern SoundIoChannelId soundio_parse_channel_id (char * str, int str_len)
extern int soundio_channel_layout_builtin_count ()
extern bool soundio_device_equal (SoundIoDevice * a, SoundIoDevice * b)
extern void soundio_device_ref (SoundIoDevice * device)
extern int soundio_default_output_device_index (SoundIo * soundio)
extern int soundio_default_input_device_index (SoundIo * soundio)
extern SoundIoDevice * soundio_get_output_device (SoundIo * soundio, int index)
extern SoundIoDevice * soundio_get_input_device (SoundIo * soundio, int index)
extern int soundio_output_device_count (SoundIo * soundio)
extern int soundio_input_device_count (SoundIo * soundio)
extern bool soundio_device_supports_layout (SoundIoDevice * device, SoundIoChannelLayout * layout)
extern char * soundio_format_string (SoundIoFormat format)
extern int soundio_get_bytes_per_frame (SoundIoFormat format, int channel_count)
extern int soundio_get_bytes_per_sample (SoundIoFormat format)
extern void soundio_sort_channel_layouts (SoundIoChannelLayout * layouts, int layout_count)
extern SoundIoChannelLayout * soundio_best_matching_channel_layout (SoundIoChannelLayout * preferred_layouts, int preferred_layout_count, SoundIoChannelLayout * available_layouts, int available_layout_count)
extern bool soundio_channel_layout_detect_builtin (SoundIoChannelLayout * layout)
extern int soundio_channel_layout_find_channel (SoundIoChannelLayout * layout, SoundIoChannelId channel)
extern SoundIoChannelLayout * soundio_channel_layout_get_default (int channel_count)
extern SoundIoChannelLayout * soundio_channel_layout_get_builtin (int index)
extern int soundio_get_bytes_per_second (SoundIoFormat format, int channel_count, int sample_rate)
extern void soundio_device_unref (SoundIoDevice * device)


