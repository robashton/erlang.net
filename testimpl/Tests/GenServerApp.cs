using System;
using CsLib;
using CsLib.Erlang;

using InfoMsg = System.Tuple<System.String, CsLib.Erlang.Pid>;
using CallMsg = System.String;

namespace TestImpl.Tests
{

  public class MyGenServer : IHandleInfo<InfoMsg>
                           , IHandleCall<CallMsg>
  {
    Runtime runtime;

    public MyGenServer(Runtime runtime) {
      this.runtime = runtime;
    }

    public HandleInfoResult HandleInfo(HandleInfoContext ctx, InfoMsg msg) {
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

    public HandleCallResult HandleCall(HandleCallContext ctx, CallMsg msg) {
      switch(msg) {
        case "hello bob": 
          return ctx.Reply("hello joe");
        default: 
          return ctx.Reply("boobs");
      }
      return ctx.Reply("ohno");
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
