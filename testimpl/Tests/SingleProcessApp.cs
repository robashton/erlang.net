using System;
using Erlang;

namespace TestImpl.Tests
{
    public class SingleProcessApp : IApp
    {
      public Object Start()
      {
        return Process.Spawn(WorkerLoop);
      }

      ProcessResult WorkerLoop(Process ctx) {
        return ctx.Receive((Process ctx, Object msg) => WorkerLoopReceive(ctx, msg));
      }

      ProcessResult WorkerLoopReceive(Process ctx, Object msg)
      {
        switch(msg) 
        {
          case Atom cmd when cmd == "bye": 
            return ctx.Finish(new Atom("ok"));

          case Tuple<Atom, Pid, Atom> tuple when tuple.Item1 == "send_me":
            Erl.Send(tuple.Item2, tuple.Item3);
            break;

          default:
            Console.WriteLine("Unexpected message received, looping");
            break;
        }
        return ctx.Receive((Process ctx, Object msg) => WorkerLoopReceive(ctx, msg));
      }
    }
}
