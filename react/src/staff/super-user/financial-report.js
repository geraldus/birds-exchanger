import React from 'react';

export class FinancialReportView extends React.Component {
    constructor(props) {
        const defLabels = {
            userCount: "User count",
            innerProfit: "Inner Profit",
            activeDeposits: "Active Deposits",
            acceptedDeposits: "Accepted Deposits"
        }
        super(props)
        // Don't call this.setState() here!
        this.state = {
            userCount: 0,
            innerProfit: {},
            activeDeposit: {
                count: 0,
                items: []
            },
            acceptedDeposit: {
                count: 0,
                items: []
            }
        }
        this.webSocket = new WebSocket(props.socket)
        this.webSocket.onopen = this.webScoketOnOpen.bind(this)
        this.webSocket.onmessage = this.webScoketOnMessage.bind(this)
        this.handleJsonMessage = this.handleJsonMessage.bind(this)
        this.handleCountEvent = this.handleCountEvent.bind(this)
        // TODO: Merge incoming labels with defaults
        this.props.labels = props.labels || defLabels
    }

    webScoketOnOpen () {
        this.webSocket.send('user count')
        this.webSocket.send('inner profit stats')
        this.webSocket.send('active deposit count')
        this.webSocket.send('accepted deposit count')
    }

    webScoketOnMessage (e) {
        try {
            const j = JSON.parse(e.data)
            this.handleJsonMessage(j)
        } catch (error) {
            console.log('Socket message', e.data)
        }
    }

    handleJsonMessage (json) {
        switch (json.type) {
            case 'count-event':
                this.handleCountEvent(json.object, json.value)
                break
            default:
                console.log(json)
        }
    }

    handleCountEvent (obj, val) {
        switch (obj) {
            case 'User Count':
                this.setState({
                    userCount: val
                })
                break
            case 'Active Deposit Count':
                this.setState({
                    activeDeposit: { count: val }
                })
                break
            case 'Accepted Deposit Count':
                this.setState({
                    acceptedDeposit: { count: val }
                })
                break
            case 'Inner Profit':
                this.setState({
                    innerProfit: val
                })
                break
            default:
                console.log(obj, val)
        }

    }

    render () {
        let ipState = this.state.innerProfit
        const innerProfit = Object
                .keys(ipState)
                .map(k => {
                    let v = ipState[k] / 100
                    return (<div>+{v.toFixed(2)}&nbsp;{k}</div>)
                })
        return(<React.Fragment>
        <div className="mb-2">
            <span>{`${this.props.labels.userCount}`}: </span>
            <span>{`${this.state.userCount}`}</span>
        </div>
        <div className="mb-2">
            <div>{`${this.props.labels.innerProfit}`}: </div>
            {innerProfit}
        </div>
        <div>
            <span>{`${this.props.labels.activeDeposits}`}: </span>
            <span>{this.state.activeDeposit.count}</span>
        </div>
        <div>
            <span>{`${this.props.labels.acceptedDeposits}`}: </span>
            <span>{this.state.acceptedDeposit.count}</span>
        </div>
        </React.Fragment>)
    }
}