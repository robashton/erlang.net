using System;
using CsLib;
using CsLib.Erlang;

using Msg = System.Tuple<System.String, CsLib.Erlang.Pid>;

namespace TestImpl.Tests
{

  public class MyGenServer : IHandleInfo<Msg>
  {
    Runtime runtime;

    public MyGenServer(Runtime runtime) {
      this.runtime = runtime;
    }

    public HandleInfoResult HandleInfo(HandleInfoContext ctx, Msg msg) {
      switch(msg) {
        case ( "hello bob", _ ): 
          runtime.Send(msg.Item2, runtime.ExportAuto("hello joe"));
          break;
        case ( _, _ ): 
          runtime.Send(msg.Item2, runtime.ExportAuto("boobs"));
          break;
      }
      return ctx.NoReply();
    }
  }

    public class GenServerApp : IApp
    {
      Runtime runtime;

      public ErlNifTerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        switch(GenServer.StartLink<MyGenServer>(runtime, (ctx) => ctx.Ok(new MyGenServer(runtime)))) {
          case Tuple<Atom, Pid> result:
            return runtime.MakeTuple2( runtime.MakeAtom(result.Item1), runtime.MakePid(result.Item2) );
          default:
            throw new InvalidOperationException();
        }
      }
    }
}
