using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl
{
    public class MyApp : IApp
    {
      public int WaitCount { get; set; }

      public Object Start()
      {
        var pid = Process.Spawn(WorkerLoop);
        return new Tuple<Atom, Pid>(new Atom("ok"), pid);
      }

      ProcessResult WorkerLoop(Process ctx) {
        return ctx.Receive(5000, (Process ctx, ErlNifTerm msg) => WorkerLoopReceive(ctx, msg));
      }

      ProcessResult WorkerLoopReceive(Process ctx, ErlNifTerm msg)
      {
        if(msg.HasValue) {
          Console.WriteLine("C# received a message, allowing process to terminate \r");
          return ctx.Finish(new Atom("ok"));
        } else {
          this.WaitCount++;
          Console.WriteLine("C# timed out waiting for message, receiving again \r");
          return ctx.Receive(5000, (Process ctx, ErlNifTerm msg) => WorkerLoopReceive(ctx, msg));
        }
      }
    }
}
