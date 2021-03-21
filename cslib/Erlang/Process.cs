using System;
using CsLib;

using System.Runtime.InteropServices;

namespace CsLib.Erlang
{
  public sealed class Process
  {
    internal Process()  {}

    public static Pid Spawn(Func<Process, ProcessResult> init) {
      ErlangCallback del = (ErlNifTerm obj) => {
        var processResult = init(new Process());
        return processResult.Native;
      };
      return Erlang.Modules.DotnetProcess.Init(del); 
    }


    public ProcessResult Receive<T>(Func<Process, T, ProcessResult> callback) {
      ErlangCallback del = (ErlNifTerm obj) => {
        T converted = Erlang.Coerce<T>(obj);
        var processResult = callback(new Process(), converted);
        return processResult.Native;
      };
      
      var resource = Erlang.MakeObjectReference(del);
      var tuple = Erlang.MakeTuple2( Erlang.MakeAtom("receive"), resource);
      return new ProcessResult(tuple);
    }

    public ProcessResult Receive<T>(int timeout, Func<Process, T, ProcessResult> callback) {
      ErlangCallback del = (ErlNifTerm obj) => {
        T converted = Erlang.Coerce<T>(obj);
        var processResult = callback(new Process(), converted);
        return processResult.Native;
      };
      
      var resource = Erlang.MakeObjectReference(del);
      var tuple = Erlang.MakeTuple3(
                    Erlang.MakeAtom("receive"),
                    Erlang.MakeInt(timeout),
                    resource);
      return new ProcessResult(tuple);
    }

    public ProcessResult Finish(ErlNifTerm result) {
      var tuple = Erlang.MakeTuple2(Erlang.MakeAtom("finish"), result);
      return new ProcessResult(tuple);
    }
  }
}
