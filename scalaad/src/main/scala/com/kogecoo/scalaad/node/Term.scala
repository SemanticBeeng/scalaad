package com.kogecoo.scalaad.node

import com.kogecoo.scalaad.Shape


trait Term[S <: Shape] { val shape: S }
