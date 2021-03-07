using System;
using CsLib;
using CsLib.Types;

namespace TestImpl
{
    public class MyApp : IApp
    {
      public ErlangPid start(ErlangRuntime runtime) {
        return runtime.spawn();
      }
    }
}
