using System;
using System.Dynamic;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class Modules : DynamicObject
  {
    private Runtime runtime;

    public Modules(Runtime runtime) {
      this.runtime = runtime;
    }
    
    public override bool TryGetMember(
        GetMemberBinder binder, out object result)
    {
      result = new Functions(runtime, binder.Name);
      return true;
    }
  }
}
