using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;

namespace CsLib.Erlang
{

  [StructLayout(LayoutKind.Sequential)]
  public struct RuntimeImpl
  {
    public IntPtr spawn;
    public IntPtr writeDebug;
    public IntPtr makeAtom;
    public IntPtr makeInt;
    public IntPtr makeTuple2;
    public IntPtr makeTuple3;
    public IntPtr makePointerResource;
    public IntPtr unpackPointerResource;
    public IntPtr releasePointerResource;
    public IntPtr stringOrAtomLength;
    public IntPtr termToString;
    public IntPtr isPid;
    public IntPtr tupleLength;
    public IntPtr tupleElement;
    public IntPtr send;
  }

  public delegate ProcessResult ProcessInit(ProcessContext ctx);
  public delegate ProcessResult ProcessMsg(ProcessContext ctx, Term msg);

  public unsafe sealed class Runtime
  {
    private delegate* <ErlNifEnv, IntPtr, ErlNifTerm> spawn;
    private delegate* <ErlNifEnv, IntPtr, ErlNifTerm> writeDebug;
    private delegate* <ErlNifEnv, IntPtr, ErlNifTerm> makeAtom;
    private delegate* <ErlNifEnv, Int32, ErlNifTerm> makeInt;
    private delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm> makeTuple2;
    private delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm, ErlNifTerm> makeTuple3;
    private delegate* <ErlNifEnv, IntPtr, ErlNifTerm> makePointerResource;
    private delegate* <ErlNifEnv, ErlNifTerm, IntPtr> unpackPointerResource;
    private delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm> releasePointerResource;
    private delegate* <ErlNifEnv, ErlNifTerm, Int32> stringOrAtomLength;
    private delegate* <ErlNifEnv, IntPtr, UInt32, ErlNifTerm, Int32> termToString;
    private delegate* <ErlNifEnv, ErlNifTerm, bool> isPid;
    private delegate* <ErlNifEnv, ErlNifTerm, Int32> tupleLength;
    private delegate* <ErlNifEnv, Int32, ErlNifTerm, ErlNifTerm> tupleElement;
    private delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, int> send;

    private static ThreadLocal<ErlNifEnv> env = new ThreadLocal<ErlNifEnv>();

    internal ErlNifEnv Env() {
      return env.Value;
    }

    internal void SetEnv(ErlNifEnv value) {
      env.Value = value;
    }

    internal Runtime(IntPtr runtime) {
      RuntimeImpl* impl = (RuntimeImpl*)runtime;
      this.spawn = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm>)impl->spawn;
      this.writeDebug = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm>)impl->writeDebug;
      this.makeAtom = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm>)impl->makeAtom;
      this.makeInt = (delegate* <ErlNifEnv, Int32, ErlNifTerm>)impl->makeInt;
      this.makeTuple2 = (delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm>)impl->makeTuple2;
      this.makeTuple3 = (delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm, ErlNifTerm>)impl->makeTuple3;
      this.makePointerResource = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm >)impl->makePointerResource;
      this.unpackPointerResource = (delegate* <ErlNifEnv, ErlNifTerm, IntPtr >)impl->unpackPointerResource;
      this.releasePointerResource = (delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm >)impl->releasePointerResource;
      this.stringOrAtomLength = (delegate* <ErlNifEnv, ErlNifTerm, Int32>)impl->stringOrAtomLength;
      this.termToString = (delegate* <ErlNifEnv, IntPtr, UInt32, ErlNifTerm, Int32>)impl->termToString;
      this.isPid = (delegate* <ErlNifEnv, ErlNifTerm, bool>)impl->isPid;
      this.tupleLength = (delegate* <ErlNifEnv, ErlNifTerm, Int32>)impl->tupleLength;
      this.tupleElement = (delegate* <ErlNifEnv, Int32, ErlNifTerm, ErlNifTerm>)impl->tupleElement;
      this.send = (delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, int>)impl->send;
    }

    public Pid Spawn(ProcessInit fn)
    {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(fn);
      var result = this.spawn(Env(), ptr);
      return new Pid(this, result);
    }

    public Term WriteDebug(String str) {
      IntPtr strPtr = (IntPtr)Marshal.StringToHGlobalAnsi(str);
      var result = this.writeDebug(Env(), strPtr);
      Marshal.FreeHGlobal(strPtr);
      return new Term(this, result);
    }

    public Atom MakeAtom(String str) {
      IntPtr strPtr = (IntPtr)Marshal.StringToHGlobalAnsi(str);
      var result = this.makeAtom(Env(), strPtr);
      Marshal.FreeHGlobal(strPtr);
      return new Atom(this, result);
    }

    public Int MakeInt(Int32 value) {
      var result = this.makeInt(Env(), value);
      return new Int(this, result);
    }

    public Tuple MakeTuple2(ITerm a, ITerm b) {
      var result = this.makeTuple2(Env(), a.Native, b.Native);
      return new Tuple(this, result);
    }

    public Tuple MakeTuple3(ITerm a, ITerm b, ITerm c) {
      var result = this.makeTuple3(Env(), a.Native, b.Native, c.Native);
      return new Tuple(this, result);
    }

    public PointerResource MakePointerResource(IntPtr ptr) {
      var result = this.makePointerResource(Env(), ptr);
      return new PointerResource(this, result);
    }

    public IntPtr UnpackPointerResource(ITerm c) {
      return this.unpackPointerResource(Env(), c.Native);
    }

    public Term ReleasePointerResource(ITerm c) {
      var result = this.releasePointerResource(Env(), c.Native);
      return new Term(this, result);
    }

    public void Send(Pid target, Term term) {
      this.send(Env(), target.Native, term.Native);
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
      return this.isPid(Env(), term);
    }

    public int TupleLength(ErlNifTerm term) {
      return this.tupleLength(Env(), term);
    }

    public ErlNifTerm TupleElement(ErlNifTerm term, int element) {
      return this.tupleElement(Env(), element, term);
    }

    public String NativeToString(ErlNifTerm term) {
      int length = 0;
      if((length = this.stringOrAtomLength(Env(), term)) > 0) {
        int allocLength = length + 1;
        IntPtr ptr = Marshal.AllocHGlobal(allocLength);
        this.termToString(Env(), ptr, (uint)allocLength, term);
        String str = Marshal.PtrToStringAnsi(ptr);
        Marshal.FreeHGlobal(ptr);
        return str;
      } 
      return String.Empty;
    }
  }
}
