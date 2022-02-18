# Design notes

1. The `finally` on `Fiber.spawn` is needed if one wants to be able to
   handle a ressource (e.g. a connection) with a fiber and forget about
   it. The reason is that a fiber may get aborted before its function
   ever gets executed, so having a toplevel `Fun.protect` is not
   enough.
  
2. Should `Fiber.join` be more like `Domain.join` (which is in fact
   slightly dubious in my eyes) and propagate exceptions ? 
  
   This would allow to have `'a t -> 'a` for join. The problem is that
   you don't necessarly `join` your fibers and don't want uncaught
   exceptions to go unoticed. Also the question would remain about
   what to do in the abort case, we want aborting fibers to abort
   their own spawns not their parent, so you need a value to represent
   it.
  
   If one wants to work with exceptions a toplevel handler can simply
   propagate them with a `('a, exn) result` (again doing that by
   default is not a good idea because you don't necessarily join
   them).

3. At the moment we can't provide the counter part of `Domain.self`, because 
   the `Fiber` module is stateless. We could:
   
   1. Request it from the scheduler via an effect.
   2. Introduce state in `Fiber`.
   3. Pass it to the spawn function. 
      
   None of which is very enticing :-)
   
4. More `Fiber.unblock` function compositionality. It's difficult to
   compose event loops but we could return an `(E.t, Dur.t option)
   Either.t` on `~poll:true`. This would allow to devise an `unblock`
   that sleeps until the smallest duration two `unblock` function
   indicate.

5. There's a tension between moving the logic to the scheduler (which
   means it's more work to design your own and screw up the semantics)
   and have it built-in the primitives (notably the fiber termination
   procedure which is, for now fully under the module's control). For
   now there's no state in `Fiber` itself but it does make life more
   complicated.

6. `Funix` shows how to use `Fiber.block` with `Unix.select`. It would 
   be interesting to be able to use a fixed API but switch backends. One
   way would be to use effects in the `Fiber.block` argument and let 
   backend interprets them. This would lead to the following code 
   structure: 
   
        Evloop.run (fun () -> Fiber.run ~unblock:Evloop.unblock main)
