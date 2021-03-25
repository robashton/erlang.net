using System;

namespace Erlang
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
