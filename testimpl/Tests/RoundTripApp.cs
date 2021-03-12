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
            
          case Tuple<Pid, Pid> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakePid(tuple.Item2));
            break;

          case Tuple<Pid, Tuple<Int32, Int32>> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeTuple2( runtime.MakeInt(tuple.Item2.Item1 ), runtime.MakeInt(tuple.Item2.Item2 ) ));
            break;

          case Tuple<Pid, Tuple<Int32, String>> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeTuple2( runtime.MakeInt(tuple.Item2.Item1 ), runtime.MakeString(tuple.Item2.Item2 ) ));
            break;

          case Tuple<Pid, Tuple<Int32, Int32, Int32>> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeTuple3( 
                  runtime.MakeInt(tuple.Item2.Item1 ), 
                  runtime.MakeInt(tuple.Item2.Item2 ),
                  runtime.MakeInt(tuple.Item2.Item3 )
                  ));
            break;

          case Tuple<Pid, Tuple<Int32, String, Pid>> tuple:
            this.runtime.Send(tuple.Item1, runtime.MakeTuple3( 
                  runtime.MakeInt(tuple.Item2.Item1 ), 
                  runtime.MakeString(tuple.Item2.Item2 ),
                  runtime.MakePid(tuple.Item2.Item3 )
                  ));
            break;

          default:
            Console.WriteLine("Unexpected message received in test");
            break;
        }
        return ctx.Finish(runtime.MakeAtom("ok"));
      }
    }
}
