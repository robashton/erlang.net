using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
    public class SingleProcessApp : IApp
    {
      Runtime runtime;

      public ErlNifTerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        var pid = Process.Spawn(runtime, WorkerLoop);
        return runtime.MakeTuple2( runtime.MakeAtom("ok"), runtime.MakePid(pid) );
      }

      ProcessResult WorkerLoop(Process ctx) {
        return ctx.Receive((Process ctx, Object msg) => WorkerLoopReceive(ctx, msg));
      }

      ProcessResult WorkerLoopReceive(Process ctx, Object msg)
      {
        switch(msg) 
        {
          case Atom cmd when cmd == "bye": 
            return ctx.Finish(this.runtime.MakeAtom("ok"));

          case Tuple<Atom, Pid, Atom> tuple when tuple.Item1 == "send_me":
            this.runtime.Send(tuple.Item2, runtime.MakeAtom(tuple.Item3));
            break;

          default:
            Console.WriteLine("Unexpected message received, looping");
            break;
        }
        return ctx.Receive((Process ctx, Object msg) => WorkerLoopReceive(ctx, msg));
      }
    }
}
