using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl
{
    public class MyApp : IApp
    {
      public ITerm Start(Runtime runtime) {
        return runtime.Spawn();
      }
    }
}
