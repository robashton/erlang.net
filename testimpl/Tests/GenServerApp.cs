using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
  public class MyGenServer 
  {
    public MyGenServer() {

    }
  }
    public class GenServerApp : IApp
    {
      Runtime runtime;

      public ErlNifTerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        var pid = GenServer.StartLink<MyGenServer>(runtime, (ctx) => ctx.Ok(new MyGenServer()));
        return runtime.MakeTuple2( runtime.MakeAtom("ok"), pid);
      }
    }
}
