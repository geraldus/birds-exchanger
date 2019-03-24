import React from 'react'
import Beep from '../beep'
import _ from 'lodash'


export default class OperatorNotifier extends React.Component {
    constructor (props) {
        super(props)
        this.listenSocket = this.listenSocket.bind(this)
        this.webScoketOnMessage = this.webScoketOnMessage.bind(this)
        this.handleJsonMessage = this.handleJsonMessage.bind(this)
        this.handleUpdateEvent = this.handleUpdateEvent.bind(this)
        this.beepComponent = <Beep ref={beepC => { this.beep = beepC }} src={window.app.config.beep}/>
        this.state = {
            deposit: {
                confirmation: {
                    counter: 0
                }
            },
            withdrawal: {
                request: {
                    counter: 0
                }
            }
        }
        if (props.socket) {
            switch(props.socket.constructor) {
                case WebSocket:
                    this.listenSocket (props.socket)
                    break;
                default:
                    console.error('Warning!  Implement cases when props.socket is string containing socket address')
                    break
            }
        }
    }

    listenSocket (socket) {
        this.socket = socket
        this.socket.addEventListener('message', e => {
            this.webScoketOnMessage (e)
        })
    }

    webScoketOnMessage (e) {
        try {
            const j = JSON.parse(e.data)
            this.handleJsonMessage(j)
        } catch (error) {
            console.groupCollapsed('Non JSON socket messages')
            console.warn(error)
            console.groupEnd()
            console.log('Socket message', e)

        }
    }

    handleJsonMessage (json) {
        switch (json.type) {
            case 'update':
                this.handleUpdateEvent(json.object, json.value)
                break
            default:
                console.log('Unexpected type', json)
        }
    }

    handleUpdateEvent (obj, val) {
        switch (obj) {
            case 'Deposit User Confirmation':
                this.setState(s => {
                    let x = s.deposit.confirmation.counter
                    return({ deposit: { confirmation: { counter: x + 1 } } })
                })
                if (this.beep) this.beep.beep()
                break
            case 'Withdrawal User Request':
                this.setState(s => {
                    let x = s.withdrawal.request.counter
                    return({ withdrawal: { request: { counter: x + 1 } } })
                })
                if (this.beep) this.beep.beep()
                break
        }
    }

    render () {
        console.log(this.state)
        const confirmations = this.state.deposit.confirmation.counter
        const requests = this.state.withdrawal.request.counter
        const loc = window.location
        const anyNotifications = confirmations > 0 || requests > 0
        return (<React.Fragment>
            {anyNotifications && <div className="notify alert">
                {confirmations > 0 && <span>Новых заявок на пополнение: {confirmations}. </span>}
                {requests > 0 && <span>Новых заявок на вывод: {requests}. </span>}
                <span>Пожалуйста, </span>
                <a href={loc}>обновите ↻</a>
                <span> страницу</span>
            </div>}
            {this.beepComponent}
        </React.Fragment>)
    }
}