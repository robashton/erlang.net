using System;
using CsLib;
using CsLib.Types;

namespace TestImpl
{
    public class MyApp : IApp
    {
      public ErlangPid init(ErlangRuntime runtime) {
        return runtime.spawn();
      }
    }
}
