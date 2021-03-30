Erlang.NET
===

Look, the clue is that I released this project on April the 1st.


This project works, it is fully functional, you *can* use it, it does all the things that I said it does, I am not lazy, I'm not going to write a blog post and not write code to back it up.

*However*


# Please don't use this.

It's implemented as an Erlang NIF - there will be issues with the scheduler if you build anything real in this, the hacks to get around the various limitations of NIFS are *glorious* but the repercussions of the problems that might arise are... not.

There probably *is* a way to use some of the ideas in this library in a safe manner, some of the calls back into Erlang could probably be managed with a dedicated erlang driver, the use of the C# ASync functionality and a lot more time.

Calling into some existing C# from Erlang and using the type mapping stuff? Possibly useful, but not more so than say ... creating a web service and calling that instead.

So again, *please don't use this*, it was a maximum effort joke, technically interesting to put together but not intended for production *at all*.

