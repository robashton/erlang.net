using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
    public class RecordRoundTripApp : IApp
    {
      Runtime runtime;

      public ErlNifTerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        var pid = Process.Spawn(runtime, WorkerLoop);
        return runtime.MakeTuple2( runtime.MakeAtom("ok"), runtime.MakePid(pid) );
      }

      ProcessResult WorkerLoop(Process ctx) {
        return ctx.Receive<OurCoolRecord>(WorkerLoopReceive);
      }

      ProcessResult WorkerLoopReceive(Process ctx, OurCoolRecord msg) {
        this.runtime.Send(msg.Owner, runtime.ExportAuto(msg));
        return ctx.Finish(runtime.MakeAtom("ok"));
      }
    }

    public record OurCoolRecord 
    {
      public Pid Owner { get; init; }
      public Atom Two { get; init; }
      public Int64 Five { get; init; }
      public Int32 Four { get; init; }
      public String One { get; init; }
      public Tuple<String, String, Pid> Six { get; init; }
    }
}
