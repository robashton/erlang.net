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
    public IntPtr makeTuple2;
    public IntPtr makeTuple3;
    public IntPtr makePointerResource;
    public IntPtr unpackPointerResource;
    public IntPtr releasePointerResource;
  }

  public delegate ITerm ProcessInit();
  public delegate ITerm ProcessMsg(ITerm msg);

  public unsafe sealed class Runtime 
  {
    private delegate* <IntPtr, IntPtr, int> spawn;
    private delegate* <IntPtr, IntPtr, int> makeAtom;
    private delegate* <IntPtr, int, int, int> makeTuple2;
    private delegate* <IntPtr, int, int, int, int> makeTuple3;
    private delegate* <IntPtr, IntPtr, int> makePointerResource;
    private delegate* <IntPtr, int, IntPtr> unpackPointerResource;
    private delegate* <IntPtr, int, int> releasePointerResource;

    private ThreadLocal<IntPtr> env = new ThreadLocal<IntPtr>();

    internal void SetEnv(IntPtr env) {
      this.env.Value = env;
    }

    internal Runtime(IntPtr runtime) {
      RuntimeImpl* impl = (RuntimeImpl*)runtime;
      this.spawn = (delegate* <IntPtr, IntPtr, int>)impl->spawn;
      this.makeAtom = (delegate* <IntPtr, IntPtr, int>)impl->makeAtom;
      this.makeTuple2 = (delegate* <IntPtr, int, int, int>)impl->makeTuple2;
      this.makeTuple3 = (delegate* <IntPtr, int, int, int, int>)impl->makeTuple3;
      this.makePointerResource = (delegate* <IntPtr, IntPtr, int >)impl->makePointerResource;
      this.unpackPointerResource = (delegate* <IntPtr, int, IntPtr >)impl->unpackPointerResource;
      this.releasePointerResource = (delegate* <IntPtr, int, int >)impl->releasePointerResource;
    }
    
    public Pid Spawn(ProcessInit fn)  
    {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(fn);
      var result = this.spawn(this.env.Value, ptr);
      return new Pid(this, result);
    }

    public Atom MakeAtom(String str) {
      IntPtr strPtr = (IntPtr)Marshal.StringToHGlobalAnsi(str);
      var result = this.makeAtom(this.env.Value, strPtr);
      Marshal.FreeHGlobal(strPtr);
      return new Atom(this, result);
    }

    public Tuple MakeTuple2(ITerm a, ITerm b) {
      var result = this.makeTuple2(this.env.Value, a.Handle(), b.Handle());
      return new Tuple(this, result);
    }

    public Tuple MakeTuple3(ITerm a, ITerm b, ITerm c) {
      var result = this.makeTuple3(this.env.Value, a.Handle(), b.Handle(), c.Handle());
      return new Tuple(this, result);
    }

    public PointerResource MakePointerResource(IntPtr ptr) {
      var result = this.makePointerResource(this.env.Value, ptr);
      return new PointerResource(this, result);
    }

    public IntPtr UnpackPointerResource(ITerm c) {
      return this.unpackPointerResource(this.env.Value, c.Handle());
    }

    public Term ReleasePointerResource(ITerm c) {
      var result = this.releasePointerResource(this.env.Value, c.Handle());
      return new Term(this, result);
    }
  }
}
