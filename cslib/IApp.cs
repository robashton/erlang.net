using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface IApp
    {
      ErlNifTerm Start(Runtime runtime);
    }
}
