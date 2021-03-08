using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl
{
    public class MyApp : IApp
    {
      Runtime runtime;

      public ITerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        return runtime.Spawn(Worker); 
      }

      ITerm Worker(Runtime runtime) {
        while(true) {
          ITerm term = runtime.Receive(5000);
          if(term.HasValue) { break; }
          Console.WriteLine("Hello from C#");
        }
        return runtime.MakeAtom("ok");
      }
    }
}
