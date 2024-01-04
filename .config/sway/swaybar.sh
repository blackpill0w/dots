# Keyboard input name
keyboard_input_name="1:1:AT_Translated_Set_2_keyboard"

# Date and time
date_and_week=$(date "+%A -- %Y-%m-%d")
current_time=$(date "+%H:%M")
# Battery or charger
battery_charge=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "percentage" | awk '{print $2}')
battery_status=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "state" | awk '{print $2}')

# Audio and multimedia
audio_volume=$(pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')
audio_is_muted=$(pacmd list-sinks | awk '/muted/ { print $2 }')
media_artist=$(pacmd info | grep 'media.artist' | cut -d'=' -f2 | xargs)
media_song=$(pacmd info | grep 'media.title' | cut -d'=' -f2 | xargs)
player_status=$(pacmd list-sink-inputs | grep -c 'state: RUNNING')

# Others
language=$(swaymsg -r -t get_inputs | awk '/1:1:AT_Translated_Set_2_keyboard/;/xkb_active_layout_name/' | grep -A1 '\b1:1:AT_Translated_Set_2_keyboard\b' | grep "xkb_active_layout_name" | awk -F '"' '{print $4}')

if [ $battery_status = "discharging" ];
then
    battery_pluggedin='⚠'
else
    battery_pluggedin='⚡'
fi

if [ $player_status -ge 1 ]
then
    song_status='⏸'
elif [ $player_status = "0" ]
then
    song_status='▶'
else
    song_status='⏹'
fi

if [ $audio_is_muted = "yes" ]
then
    audio_active='🔇'
else
    audio_active='🔊'
fi

echo "🎧 $song_status $media_artist - $media_song | ⌨ $language | $audio_active $audio_volume% | $battery_pluggedin $battery_charge | $date_and_week | 🕘 $current_time"
