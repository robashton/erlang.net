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
  public delegate ProcessResult ProcessMsg(ProcessContext ctx, Term msg);

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

    public Pid Spawn(ProcessInit fn)
    {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(fn);
      var result = Imports.erldotnet_spawn(Env(), ptr);
      return new Pid(this, result);
    }

    public Term WriteDebug(String value) {
      var result = Imports.erldotnet_write_debug(Env(), new StringBuilder(value));
      return new Term(this, result);
    }

    public Atom MakeAtom(String value) {
      var result = Imports.erldotnet_make_atom(Env(), new StringBuilder(value));
      return new Atom(this, result);
    }

    public Int MakeInt(Int32 value) {
      var result = Imports.erldotnet_make_int(Env(), value);
      return new Int(this, result);
    }

    public Tuple MakeTuple2(ITerm a, ITerm b) {
      var result = Imports.erldotnet_make_tuple2(Env(), a.Native, b.Native);
      return new Tuple(this, result);
    }

    public Tuple MakeTuple3(ITerm a, ITerm b, ITerm c) {
      var result = Imports.erldotnet_make_tuple3(Env(), a.Native, b.Native, c.Native);
      return new Tuple(this, result);
    }

    public PointerResource MakePointerResource(IntPtr ptr) {
      var result = Imports.erldotnet_make_pointer_resource(Env(), ptr);
      return new PointerResource(this, result);
    }

    public IntPtr UnpackPointerResource(ITerm c) {
      return Imports.erldotnet_unpack_pointer_resource(Env(), c.Native);
    }

    public Term ReleasePointerResource(ITerm c) {
      var result = Imports.erldotnet_release_pointer_resource(Env(), c.Native);
      return new Term(this, result);
    }

    public void Send(Pid target, Term term) {
      Imports.erldotnet_send(Env(), target.Native, term.Native);
    }

    public T Coerce<T>(ErlNifTerm term) {
      object result = Coerce(term, typeof(T));
      if(result == null) {
        return default(T);
      }
      return (T)result;
    }

    public object Coerce(ErlNifTerm term, Type type) {
      if(type == typeof(String)) {
        return NativeToString(term);
      }
      if(type == typeof(Pid)) {
        return NativeToPid(term);
      }
      if(type == typeof(Term)) {
        return new Term(this, term);
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

    public Pid NativeToPid(ErlNifTerm term) {
      if(IsPid(term)) {
        return new Pid(this, term);
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
