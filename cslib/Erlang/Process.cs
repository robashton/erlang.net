using System;
using CsLib;

using System.Runtime.InteropServices;

namespace CsLib.Erlang
{
  public delegate ProcessResult ProcessInit(Process ctx);
  public delegate ProcessResult ProcessMsg(Process ctx, ErlNifTerm msg);
  public delegate ProcessResult ProcessMsg<T>(Process ctx, T msg);

  public sealed class Process
  {
    Runtime runtime;
    internal Process(Runtime runtime)  {
      this.runtime = runtime;
    }

    public static Pid Spawn(Runtime runtime, ProcessInit init) {
      ErlangCallback del = (Runtime runtime, ErlNifTerm obj) => {
        var processResult = init(new Process(runtime));
        return processResult.Native;
      };
      var resource = runtime.MakeObjectReference(del);
      var result = runtime.CallErlangFn("dotnetprocess", "init", new [] { resource }); 
      return runtime.Coerce<Pid>(result);
    }


    public ProcessResult Receive<T>(ProcessMsg<T> callback) {
      ErlangCallback del = (Runtime runtime, ErlNifTerm obj) => {
        T converted = runtime.Coerce<T>(obj);
        var processResult = callback(new Process(runtime), converted);
        return processResult.Native;
      };
      
      var resource = runtime.MakeObjectReference(del);
      var tuple = runtime.MakeTuple2( this.runtime.MakeAtom("receive"), resource);
      return new ProcessResult(this.runtime, tuple);
    }

    public ProcessResult Receive<T>(int timeout, ProcessMsg<T> callback) {
      ErlangCallback del = (Runtime runtime, ErlNifTerm obj) => {
        T converted = runtime.Coerce<T>(obj);
        var processResult = callback(new Process(runtime), converted);
        return processResult.Native;
      };
      
      var resource = runtime.MakeObjectReference(del);
      var tuple = this.runtime.MakeTuple3(
                    this.runtime.MakeAtom("receive"),
                    this.runtime.MakeInt(timeout),
                    resource);
      return new ProcessResult(this.runtime, tuple);
    }

    public ProcessResult Finish(ErlNifTerm result) {
      var tuple = this.runtime.MakeTuple2(this.runtime.MakeAtom("finish"), result);
      return new ProcessResult(this.runtime, tuple);
    }
  }
}
