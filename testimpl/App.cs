﻿using System;
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

      ITerm WorkerLoop(ProcessContext ctx) {
        return ctx.Receive(5000, WorkerLoopReceive);
      }

      ITerm WorkerLoopReceive(ProcessContext ctx, ITerm msg)
      {
        if(msg != null) {
          Console.WriteLine("C# received a message, allowing process to terminate \r");
          return ctx.Finish(this.runtime.MakeAtom("ok"));
        } else {
          this.WaitCount++;
          Console.WriteLine("C# timed out waiting for message, receiving again \r");
          return ctx.Receive(5000, WorkerLoopReceive);
        }
      }
    }
}
