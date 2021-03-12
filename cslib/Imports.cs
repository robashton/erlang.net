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
    internal static extern ErlNifTerm erldotnet_call_erlang_fn(ErlNifEnv env, ErlNifTerm mfa);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_write_debug(ErlNifEnv env, StringBuilder value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_atom(ErlNifEnv env, StringBuilder value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_int(ErlNifEnv env, Int32 value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_int64(ErlNifEnv env, Int64 value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_string(ErlNifEnv env, StringBuilder value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_tuple2(ErlNifEnv env, ErlNifTerm a, ErlNifTerm b);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_tuple3(ErlNifEnv env, ErlNifTerm a, ErlNifTerm b, ErlNifTerm c);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_list1(ErlNifEnv env, ErlNifTerm a);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_listn(ErlNifEnv env, uint len, ErlNifTerm[] a);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_make_pid(ErlNifEnv env, Pid value);

    [DllImport("erldotnet")]
    internal static unsafe extern ErlNifTerm erldotnet_make_pointer_resource(ErlNifEnv env, delegate* <IntPtr, ErlNifTerm> @return, IntPtr ptr);

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
    internal static extern Int32 erldotnet_term_to_int32(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern Int64 erldotnet_term_to_int64(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern int erldotnet_tuple_length(ErlNifEnv env, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern ErlNifTerm erldotnet_tuple_element(ErlNifEnv env, int index, ErlNifTerm value);

    [DllImport("erldotnet")]
    internal static extern Pid erldotnet_get_pid(ErlNifEnv env, ErlNifTerm value);
  }
}
