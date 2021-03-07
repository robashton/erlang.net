using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface IApp
    {
      ITerm Start(Runtime runtime);
    }
}
