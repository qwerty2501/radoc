package net.qwerty2501.radoc

private object ParameterColor {

  def color(): Color = {
    val index = currentIndex
    currentIndex += 1
    parameterColors(index)
  }

  private var currentIndex = 0

  private val parameterColors: RotationParameterColorList[Color] =
    RotationParameterColorList(
      Seq(
        Color.lightSeaGreen,
        Color.dodgerBlue,
        Color.lightSalmon,
        Color.paleVioletRed,
        Color.chocolate,
        Color.fuchsia,
        Color.cyan,
        Color.darkOrange,
        Color.aqua,
        Color.paleVioletRed,
      )
    )
}
