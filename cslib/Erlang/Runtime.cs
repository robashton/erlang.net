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
    private delegate* <IntPtr, IntPtr, ErlNifTerm> spawn;
    private delegate* <IntPtr, IntPtr, ErlNifTerm> makeAtom;
    private delegate* <IntPtr, Int32, ErlNifTerm> makeInt;
    private delegate* <IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm> makeTuple2;
    private delegate* <IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm, ErlNifTerm> makeTuple3;
    private delegate* <IntPtr, IntPtr, ErlNifTerm> makePointerResource;
    private delegate* <IntPtr, ErlNifTerm, IntPtr> unpackPointerResource;
    private delegate* <IntPtr, ErlNifTerm, ErlNifTerm> releasePointerResource;

    // TODO:
    private static ThreadLocal<IntPtr> env = new ThreadLocal<IntPtr>();

    internal void SetEnv(IntPtr value) {
      env.Value = value;
    }

    internal Runtime(IntPtr runtime) {
      RuntimeImpl* impl = (RuntimeImpl*)runtime;
      this.spawn = (delegate* <IntPtr, IntPtr, ErlNifTerm>)impl->spawn;
      this.makeAtom = (delegate* <IntPtr, IntPtr, ErlNifTerm>)impl->makeAtom;
      this.makeInt = (delegate* <IntPtr, Int32, ErlNifTerm>)impl->makeInt;
      this.makeTuple2 = (delegate* <IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm>)impl->makeTuple2;
      this.makeTuple3 = (delegate* <IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm, ErlNifTerm>)impl->makeTuple3;
      this.makePointerResource = (delegate* <IntPtr, IntPtr, ErlNifTerm >)impl->makePointerResource;
      this.unpackPointerResource = (delegate* <IntPtr, ErlNifTerm, IntPtr >)impl->unpackPointerResource;
      this.releasePointerResource = (delegate* <IntPtr, ErlNifTerm, ErlNifTerm >)impl->releasePointerResource;
    }

    public Pid Spawn(ProcessInit fn)
    {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(fn);
      var result = this.spawn(env.Value, ptr);
      Console.WriteLine("Spawn done");
      return new Pid(this, result);
    }

    public Atom MakeAtom(String str) {
      IntPtr strPtr = (IntPtr)Marshal.StringToHGlobalAnsi(str);
      var result = this.makeAtom(env.Value, strPtr);
      Marshal.FreeHGlobal(strPtr);
      return new Atom(this, result);
    }

    public Int MakeInt(Int32 value) {
      var result = this.makeInt(env.Value, value);
      return new Int(this, result);
    }

    public Tuple MakeTuple2(ITerm a, ITerm b) {
      var result = this.makeTuple2(env.Value, a.Native, b.Native);

      Console.WriteLine("wrapping up a tuple with " + result.ToString());

      return new Tuple(this, result);
    }

    public Tuple MakeTuple3(ITerm a, ITerm b, ITerm c) {
      var result = this.makeTuple3(env.Value, a.Native, b.Native, c.Native);
      return new Tuple(this, result);
    }

    public PointerResource MakePointerResource(IntPtr ptr) {
      var result = this.makePointerResource(env.Value, ptr);
      return new PointerResource(this, result);
    }

    public IntPtr UnpackPointerResource(ITerm c) {
      return this.unpackPointerResource(env.Value, c.Native);
    }

    public Term ReleasePointerResource(ITerm c) {
      var result = this.releasePointerResource(env.Value, c.Native);
      return new Term(this, result);
    }
  }
}
