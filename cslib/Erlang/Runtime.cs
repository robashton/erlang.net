using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;
using CsLib;

namespace CsLib.Erlang
{
  public delegate ProcessResult ProcessInit(ProcessContext ctx);
  public delegate ProcessResult ProcessMsg(ProcessContext ctx, ErlNifTerm msg);

  public unsafe sealed class Runtime
  {
    private static ThreadLocal<ErlNifEnv> env = new ThreadLocal<ErlNifEnv>();

    internal ErlNifEnv Env() {
      return env.Value;
    }

    internal void SetEnv(ErlNifEnv value) {
      env.Value = value;
    }

    internal Runtime() {
    }

    public ErlNifTerm Spawn(ProcessInit fn)
    {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(fn);
      return Imports.erldotnet_spawn(Env(), ptr);
    }

    public ErlNifTerm WriteDebug(String value) {
      return Imports.erldotnet_write_debug(Env(), new StringBuilder(value));
    }

    public ErlNifTerm MakeAtom(String value) {
      return Imports.erldotnet_make_atom(Env(), new StringBuilder(value));
    }

    public ErlNifTerm MakeInt(Int32 value) {
      return Imports.erldotnet_make_int(Env(), value);
    }

    public ErlNifTerm MakeTuple2(ErlNifTerm a, ErlNifTerm b) {
      return Imports.erldotnet_make_tuple2(Env(), a, b);
    }

    public ErlNifTerm MakeTuple3(ErlNifTerm a, ErlNifTerm b, ErlNifTerm c) {
      return Imports.erldotnet_make_tuple3(Env(), a, b, c);
    }

    public ErlNifTerm MakePointerResource(IntPtr ptr) {
      return Imports.erldotnet_make_pointer_resource(Env(), ptr);
    }

    public IntPtr UnpackPointerResource(ErlNifTerm c) {
      return Imports.erldotnet_unpack_pointer_resource(Env(), c);
    }

    public ErlNifTerm ReleasePointerResource(ErlNifTerm c) {
      return Imports.erldotnet_release_pointer_resource(Env(), c);
    }

    public void Send(Pid target, ErlNifTerm term) {
      Imports.erldotnet_send(Env(), target, term);
    }

    public T Coerce<T>(ErlNifTerm term) {
      object result = Coerce(term, typeof(T));
      if(result == null) {
        return default(T);
      }
      return (T)result;
    }
    
    public object ExtractAuto(ErlNifTerm term) {
      Type t = DeriveType(term);
      return Coerce(term, t);
    }

    public Type DeriveType(ErlNifTerm term)  {
      if(Imports.erldotnet_is_tuple(Env(), term)) {
        var length = Imports.erldotnet_tuple_length(Env(), term);
        var typeArguments = Enumerable.Range(0, length)
          .Select(i =>  DeriveType(Imports.erldotnet_tuple_element(Env(), i, term)))
          .ToArray();

        var method =  typeof(System.Tuple).GetMethods()
                         .FirstOrDefault(method => method.Name == "Create" && method.GetParameters().Length == typeArguments.Length)
                         .MakeGenericMethod(typeArguments);

        return method.ReturnType;
      }

      // TF this is only an elaborate April Fools
      // and I don't have to actually support this code in any way
      if(Imports.erldotnet_is_atom(Env(), term)) {
        return typeof(Atom);
      }
      if(Imports.erldotnet_is_double(Env(), term)) {
        return typeof(double);
      }
      if(Imports.erldotnet_is_number(Env(), term)) {
        if(Imports.erldotnet_is_int32(Env(), term)) {
          return typeof(Int32);
        }
        if(Imports.erldotnet_is_int64(Env(), term)) {
          return typeof(Int64);
        }
        return typeof(ErlNifTerm);
      }
      if(Imports.erldotnet_is_pid(Env(), term)) {
        return typeof(Pid);
      }
      if(Imports.erldotnet_is_string(Env(), term)) {
        return typeof(String);
      }
      if(Imports.erldotnet_is_binary(Env(), term)) {
        return typeof(byte[]);
      }
      return typeof(ErlNifTerm);
    }
     
    public object Coerce(ErlNifTerm term, Type type) {
      if(type == typeof(String)) {
        return NativeToString(term);
      }
      if(type == typeof(Pid)) {
        return Imports.erldotnet_get_pid(Env(), term);
      }

      if(type == typeof(Atom)) {
        return new Atom(Coerce<String>(term));
      }
      if(type == typeof(ErlNifTerm)) {
        return term;
      }

      var tupleTypes = new Type[]
      { typeof(Tuple<,>),
        typeof(Tuple<,,>),
        typeof(Tuple<,,,>),
        typeof(Tuple<,,,,>),
        typeof(Tuple<,,,,,>),
        typeof(Tuple<,,,,,,>),
        typeof(Tuple<,,,,,,,>),
      };

      if(type.IsGenericType && tupleTypes.Any(x => x == type.GetGenericTypeDefinition())) {
        var argumentTypes = type.GetGenericArguments();
        var tupleLength = TupleLength(term);
        if(tupleLength < 0) {
          return null;
        }

        var convertedArgs = argumentTypes.Select((t, i) => Coerce(TupleElement(term, i), t));

        ConstructorInfo method = type.GetConstructor(argumentTypes);

        if(method == null) {
          return null;
        }

        return method.Invoke(convertedArgs.ToArray());
      }
      return null;
    }

    public bool IsPid(ErlNifTerm term) {
      return Imports.erldotnet_is_pid(Env(), term);
    }

    public int TupleLength(ErlNifTerm term) {
      return Imports.erldotnet_tuple_length(Env(), term);
    }

    public ErlNifTerm TupleElement(ErlNifTerm term, int element) {
      return Imports.erldotnet_tuple_element(Env(), element, term);
    }

    public String NativeToString(ErlNifTerm term) {
      int length = 0;
      if((length = Imports.erldotnet_string_or_atom_length(Env(), term)) > 0) {
        int allocLength = length + 1;
        IntPtr ptr = Marshal.AllocHGlobal(allocLength);
        Imports.erldotnet_term_to_string(Env(), ptr, (uint)allocLength, term);
        String str = Marshal.PtrToStringAnsi(ptr);
        Marshal.FreeHGlobal(ptr);
        return str;
      }
      return String.Empty;
    }
  }
}
