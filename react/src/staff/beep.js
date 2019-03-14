import React from 'react'

export default class Beep extends React.Component {
    constructor (props) {
        super(props)
        this.audio = new Audio(props.src)
        this.beep = this.beep.bind(this)
    }

    beep () {
        let playPromise = this.audio.play()
        if (playPromise !== undefined) {
            playPromise.then(_ => {
              // Automatic playback started!
              // Show playing UI
            })
            .catch(error => {
              // Auto-play was prevented
              // Show paused UI.
              console.warn(error)

              alert('Чтобы слышать звуковые уведомления перейдите на главную страницу, а потом вернитесь')
            });
          }
    }

    render () { return (null) }
}