using System;
using CsLib.Erlang;
using CsLib;

namespace TestImpl.Tests
{
    public class RoundTripApp : IApp
    {
      public Object Start()
      {
        return Process.Spawn(WorkerLoop);
      }

      ProcessResult WorkerLoop(Process ctx) {
        return ctx.Receive<Object>(WorkerLoopReceive);
      }

      ProcessResult WorkerLoopReceive(Process ctx, Object msg)
      {
        switch(msg) 
        {
          case Tuple<Pid, Atom> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2);
            break;

          case Tuple<Pid, Int32> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2);
            break;

          case Tuple<Pid, Int64> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2);
            break;

          case Tuple<Pid, String> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2);
            break;
            
          case Tuple<Pid, Pid> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2);
            break;

          case Tuple<Pid, Tuple<Int32, Int32>> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2); 
            break;

          case Tuple<Pid, Tuple<Int32, String>> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2);
            break;

          case Tuple<Pid, Tuple<Int32, Int32, Int32>> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2); 
            break;

          case Tuple<Pid, Tuple<Int32, String, Pid>> tuple:
            Erlang.Send(tuple.Item1, tuple.Item2); 
            break;

          default:
            Console.WriteLine("Unexpected message received in test");
            break;
        }
        return ctx.Receive<Object>(WorkerLoopReceive);
      }
    }
}
