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
        var pid = GenServer.StartLink(runtime, (ctx) => new MyGenServer());
        return runtime.MakeTuple2( runtime.MakeAtom("ok"), runtime.MakePid(pid));
      }
    }
}
