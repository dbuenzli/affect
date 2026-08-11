(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Action = Affect__action
module Fun = struct
  include Stdlib.Fun
  module Async = struct
    include Affect__base.Panic
    include Affect__async_fun
    include Affect__scheduler
    module Trace = Affect__trace
  end
end
module Port = Affect__port
module Cell = Affect__cell
module Semaphore = Affect__semaphore
