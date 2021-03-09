using System;
using CsLib;

using System.Runtime.InteropServices;

namespace CsLib.Erlang 
{
  public sealed class ProcessContext
  {
    Runtime runtime;
    internal ProcessContext(Runtime runtime)  {
      this.runtime = runtime;
    }

    public ITerm Receive(ProcessMsg callback) {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(callback);
      ITerm ptrResource = this.runtime.MakePointerResource(ptr);
      var tuple = this.runtime.MakeTuple2(
                    this.runtime.MakeAtom("receive"),
                    ptrResource);
      return new ProcessResult(this.runtime, tuple.Handle());
    }

    public ITerm Receive(int timeout, ProcessMsg callback) {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(callback);
      ITerm ptrResource = this.runtime.MakePointerResource(ptr);
      var tuple = this.runtime.MakeTuple3(
                    this.runtime.MakeAtom("receive"),
                    this.runtime.MakeInt(timeout),
                    ptrResource);
      return new ProcessResult(this.runtime, tuple.Handle());
    }

    public ProcessResult Finish(ITerm result) {
      var tuple = this.runtime.MakeTuple2(this.runtime.MakeAtom("finish"), result);
      return new ProcessResult(this.runtime, tuple.Handle());
    }
  }
}
