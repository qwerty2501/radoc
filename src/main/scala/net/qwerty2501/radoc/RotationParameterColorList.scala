package net.qwerty2501.radoc

private case class RotationParameterColorList[TColor](
    private val seq: Seq[TColor]) {
  def apply(index: Int): TColor =
    if (index >= seq.length) apply(index - seq.length) else seq(index)
}
