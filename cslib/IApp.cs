using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface IApp
    {
      Object Start(Runtime runtime);
    }
}
