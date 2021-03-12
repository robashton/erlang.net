using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
    public class RoundTripApp : IApp
    {
      Runtime runtime;

      public ErlNifTerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        var pid = runtime.Spawn(WorkerLoop);
        return runtime.MakeTuple2( runtime.MakeAtom("ok"), pid );
      }

      ProcessResult WorkerLoop(ProcessContext ctx) {
        return ctx.Receive((ProcessContext ctx, Object msg) => WorkerLoopReceive(ctx, msg));
      }

      ProcessResult WorkerLoopReceive(ProcessContext ctx, Object msg)
      {
        switch(msg) 
        {
          case Tuple<Pid, Atom> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeAtom(tuple.Item2));
            break;

          case Tuple<Pid, Int32> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeInt(tuple.Item2));
            break;

          case Tuple<Pid, Int64> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeInt64(tuple.Item2));
            break;

          case Tuple<Pid, String> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeString(tuple.Item2));
            break;

          default:
            Console.WriteLine("Unexpected message received in test");
            break;
        }
        return ctx.Finish(runtime.MakeAtom("ok"));
      }
    }
}
