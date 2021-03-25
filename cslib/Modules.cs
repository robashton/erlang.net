using System;
using System.Dynamic;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;

namespace Erlang
{
  public sealed class Modules : DynamicObject
  {
    public Modules() {
    }
    
    public override bool TryGetMember(
        GetMemberBinder binder, out object result)
    {
      result = new Functions(binder.Name);
      return true;
    }
  }
}
