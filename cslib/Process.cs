using System;
using System.Runtime.InteropServices;

namespace Erlang
{
  public sealed class Process
  {
    internal Process()  {}

    public static Pid Spawn(Func<Process, ProcessResult> init) {
      ErlangCallback del = (ErlNifTerm obj) => {
        var processResult = init(new Process());
        return processResult.Native;
      };
      return Erl.Modules.DotnetProcess.Init(del); 
    }


    public ProcessResult Receive<T>(Func<Process, T, ProcessResult> callback) {
      ErlangCallback del = (ErlNifTerm obj) => {
        T converted = Erl.Coerce<T>(obj);
        var processResult = callback(new Process(), converted);
        return processResult.Native;
      };
      
      var resource = Erl.MakeObjectReference(del);
      var tuple = Erl.MakeTuple2( Erl.MakeAtom("receive"), resource);
      return new ProcessResult(tuple);
    }

    public ProcessResult Receive<T>(int timeout, Func<Process, T, ProcessResult> callback) {
      ErlangCallback del = (ErlNifTerm obj) => {
        T converted = Erl.Coerce<T>(obj);
        var processResult = callback(new Process(), converted);
        return processResult.Native;
      };
      
      var resource = Erl.MakeObjectReference(del);
      var tuple = Erl.MakeTuple3(
                    Erl.MakeAtom("receive"),
                    Erl.MakeInt(timeout),
                    resource);
      return new ProcessResult(tuple);
    }

    public ProcessResult Finish(Object result) {
      return Finish(Erl.ExportAuto(result));
    }
    public ProcessResult Finish(ErlNifTerm result) {
      var tuple = Erl.MakeTuple2(Erl.MakeAtom("finish"), result);
      return new ProcessResult(tuple);
    }
  }
}
