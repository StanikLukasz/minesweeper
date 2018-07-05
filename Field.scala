import scala.util.Random

import State._


class Field (var state: State = Empty, var value: Int = 0, var hidden: Boolean = true, var checked: Boolean = false)