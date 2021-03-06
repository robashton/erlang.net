using System;
using CsLib.Types;

namespace CsLib
{
    public interface IApp
    {
      ErlangPid init(ErlangRuntime runtime);
    }
}
