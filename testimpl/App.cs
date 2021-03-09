using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl
{
    public class MyApp : IApp
    {
      Runtime runtime;

      public int WaitCount { get; set; }

      public ITerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        return runtime.Spawn(WorkerLoop);
      }

      ProcessResult WorkerLoop(ProcessContext ctx) {
        Console.WriteLine("In WorkerLoop (Init)");
        return ctx.Receive(5000, WorkerLoopReceive);
      }

      ProcessResult WorkerLoopReceive(ProcessContext ctx, ITerm msg)
      {
        if(msg.HasValue) {
          Console.WriteLine("C# received a message, allowing process to terminate");
          return ctx.Finish(this.runtime.MakeAtom("ok"));
        } else {
          this.WaitCount++;
          Console.WriteLine("C# timed out waiting for message, receiving again");
          return ctx.Receive(5000, WorkerLoopReceive);
        }
      }
    }
}
