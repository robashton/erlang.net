using System;
using System.Runtime.InteropServices;
using System.Threading;

namespace CsLib.Erlang
{

  [StructLayout(LayoutKind.Sequential)]
  public struct RuntimeImpl
  {
    public IntPtr spawn;
    public IntPtr makeAtom;
    public IntPtr makeInt;
    public IntPtr makeTuple2;
    public IntPtr makeTuple3;
    public IntPtr makePointerResource;
    public IntPtr unpackPointerResource;
    public IntPtr releasePointerResource;
  }

  public delegate ITerm ProcessInit(ProcessContext ctx);
  public delegate ITerm ProcessMsg(ProcessContext ctx, ITerm msg);

  public unsafe sealed class Runtime
  {
    private delegate* <ErlNifEnv, IntPtr, ErlNifTerm> spawn;
    private delegate* <ErlNifEnv, IntPtr, ErlNifTerm> makeAtom;
    private delegate* <ErlNifEnv, Int64, ErlNifTerm> makeInt;
    private delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm> makeTuple2;
    private delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm, ErlNifTerm> makeTuple3;
    private delegate* <ErlNifEnv, IntPtr, ErlNifTerm> makePointerResource;
    private delegate* <ErlNifEnv, ErlNifTerm, IntPtr> unpackPointerResource;
    private delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm> releasePointerResource;

    // TODO:
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
      this.makeAtom = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm>)impl->makeAtom;
      this.makeInt = (delegate* <ErlNifEnv, Int64, ErlNifTerm>)impl->makeInt;
      this.makeTuple2 = (delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm>)impl->makeTuple2;
      this.makeTuple3 = (delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm, ErlNifTerm, ErlNifTerm>)impl->makeTuple3;
      this.makePointerResource = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm >)impl->makePointerResource;
      this.unpackPointerResource = (delegate* <ErlNifEnv, ErlNifTerm, IntPtr >)impl->unpackPointerResource;
      this.releasePointerResource = (delegate* <ErlNifEnv, ErlNifTerm, ErlNifTerm >)impl->releasePointerResource;
    }

    public Pid Spawn(ProcessInit fn)
    {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(fn);
      var result = this.spawn(Env(), ptr);
      Console.WriteLine("Spawn done");
      return new Pid(this, result);
    }

    public Atom MakeAtom(String str) {
      IntPtr strPtr = (IntPtr)Marshal.StringToHGlobalAnsi(str);
      var result = this.makeAtom(Env(), strPtr);
      Marshal.FreeHGlobal(strPtr);
      return new Atom(this, result);
    }

    public Int MakeInt(Int64 value) {
      var result = this.makeInt(Env(), value);
      return new Int(this, result);
    }

    public Tuple MakeTuple2(ITerm a, ITerm b) {
      var result = this.makeTuple2(Env(), a.Native, b.Native);

      Console.WriteLine("wrapping up a tuple with " + result.ToString());

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
  }
}
