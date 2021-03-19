using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
    public class RoundTripApp : IApp
    {
      Runtime runtime;

      public Object Start(Runtime runtime)
      {
        this.runtime = runtime;
        return Process.Spawn(runtime, WorkerLoop);
      }

      ProcessResult WorkerLoop(Process ctx) {
        return ctx.Receive<Object>(WorkerLoopReceive);
      }

      ProcessResult WorkerLoopReceive(Process ctx, Object msg)
      {
        switch(msg) 
        {
          case Tuple<Pid, Atom> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2));
            break;

          case Tuple<Pid, Int32> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2));
            break;

          case Tuple<Pid, Int64> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2));
            break;

          case Tuple<Pid, String> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2));
            break;
            
          case Tuple<Pid, Pid> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2));
            break;

          case Tuple<Pid, Tuple<Int32, Int32>> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2)); 
            break;

          case Tuple<Pid, Tuple<Int32, String>> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2));
            break;

          case Tuple<Pid, Tuple<Int32, Int32, Int32>> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2)); 
            break;

          case Tuple<Pid, Tuple<Int32, String, Pid>> tuple:
            this.runtime.Send(tuple.Item1, runtime.ExportAuto(tuple.Item2)); 
            break;

          default:
            Console.WriteLine("Unexpected message received in test");
            break;
        }
        return ctx.Receive<Object>(WorkerLoopReceive);
      }
    }
}
