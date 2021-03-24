using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface IApp
    {
      Object Start();
    }

    public interface IApp<TArgs>
    {
      Object Start(TArgs args);
    }
}
