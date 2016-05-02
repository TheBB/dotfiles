function conky_title(title)
  return string.format('${font Arial:size=11:weight=bold}${color Tan1}%s ' ..
                       '${color gray}${voffset -2}${hr 2}$font$color\n' ..
                       '${voffset -6}', title)
end
