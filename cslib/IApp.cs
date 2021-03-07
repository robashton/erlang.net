using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface IApp
    {
      Pid start(Runtime runtime);
    }
}
