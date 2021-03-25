using System;
using Erlang;

namespace TestImpl.Tests
{
    public class RecordRoundTripApp : IApp
    {
      public Object Start()
      {
        return Process.Spawn((ctx) => ctx.Receive<OurCoolRecord>((WorkerLoopReceive)));
      }

      ProcessResult WorkerLoopReceive(Process ctx, OurCoolRecord msg) {
        Erl.Send(msg.Owner, msg);
        return ctx.Finish(Erl.MakeAtom("ok"));
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
