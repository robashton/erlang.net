using System;
using System.Runtime.InteropServices;
using System.Text;

namespace CsLib.Erlang
{
  internal static class Imports {

    // TODO: Probably get rid of this function
    // we can achieve the same by using tuple-building and 
    // a remote 'call'
    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_spawn(ErlNifEnv env, IntPtr fn);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_write_debug(ErlNifEnv env, StringBuilder value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_atom(ErlNifEnv env, StringBuilder value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_int(ErlNifEnv env, Int32 value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_tuple2(ErlNifEnv env, ErlNifTerm a, ErlNifTerm b);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_tuple3(ErlNifEnv env, ErlNifTerm a, ErlNifTerm b, ErlNifTerm c);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_pointer_resource(ErlNifEnv env, IntPtr ptr);

    [DllImport("erldotnet")]
    internal static extern IntPtr erldotnet_unpack_pointer_resource(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_release_pointer_resource(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_send(ErlNifEnv env, Pid pid, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_tuple(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_pid(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_atom(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_double(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_number(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_int32(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_int64(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_string(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern bool erldotnet_is_binary(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern int erldotnet_string_or_atom_length(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern int erldotnet_term_to_string(ErlNifEnv env, IntPtr buffer, UInt32 bufferLength, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern int erldotnet_tuple_length(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_tuple_element(ErlNifEnv env, int index, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern Pid erldotnet_get_pid(ErlNifEnv env, ErlNifTerm value);
  }
}
