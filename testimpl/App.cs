using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl
{
    public class MyApp : IApp
    {
      Runtime runtime;

      public int WaitCount { get; set; }

      public ErlNifTerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        var pid = Process.Spawn(runtime, WorkerLoop);
        return runtime.MakeTuple2(runtime.MakeAtom("ok"), runtime.MakePid(pid));
      }

      ProcessResult WorkerLoop(Process ctx) {
        return ctx.Receive(5000, (Process ctx, ErlNifTerm msg) => WorkerLoopReceive(ctx, msg));
      }

      ProcessResult WorkerLoopReceive(Process ctx, ErlNifTerm msg)
      {
        if(msg.HasValue) {
          Console.WriteLine("C# received a message, allowing process to terminate \r");
          return ctx.Finish(this.runtime.MakeAtom("ok"));
        } else {
          this.WaitCount++;
          Console.WriteLine("C# timed out waiting for message, receiving again \r");
          return ctx.Receive(5000, (Process ctx, ErlNifTerm msg) => WorkerLoopReceive(ctx, msg));
        }
      }
    }
}
