import React from 'react';

export class FinancialReportView extends React.Component {
    constructor(props) {
        const defLabels = {
            userCount: "User count",
            innerProfit: "Inner Profit",
            activeDeposits: "Active Deposits",
            acceptedDeposits: "Accepted Deposits",
            deposit: {
                income: {
                    realTotal: "Deposited Money",
                    feeTotal: "Fee Collected"
                }
            }
        }
        super(props)
        this.props.labels = { ...defLabels, ...props.labels }
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
            },
            deposit: {
                income: {
                    real: {},
                    fee: {}
                }
            }
        }
        this.webSocket = new WebSocket(props.socket)
        this.webSocket.onopen = this.webScoketOnOpen.bind(this)
        this.webSocket.onmessage = this.webScoketOnMessage.bind(this)
        this.handleJsonMessage = this.handleJsonMessage.bind(this)
        this.handleCountEvent = this.handleCountEvent.bind(this)
    }

    webScoketOnOpen () {
        this.webSocket.send('user count')
        this.webSocket.send('inner profit stats')
        this.webSocket.send('active deposit count')
        this.webSocket.send('accepted deposit count')
        this.webSocket.send('deposited money')
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
            case 'Deposited Money':
                this.setState({
                    deposit: {
                        income: {
                            real: val,
                            fee: this.state.deposit.income.fee
                        }
                    }
                })
                break
            case 'Deposit Fee':
                this.setState({
                    deposit: {
                        income: {
                            fee: val,
                            real: this.state.deposit.income.real
                        }
                    }
                })
                break
            default:
                console.log(obj, val)
        }

    }

    render () {
        console.log(this.state)
        let ipState = this.state.innerProfit
        const innerProfit = Object
                .keys(ipState)
                .map(k => {
                    let v = ipState[k] / 100
                    return (<div>+{v.toFixed(2)}&nbsp;{k}</div>)
                })
        return(<React.Fragment>
        <div className="mb-3">
            <span>{`${this.props.labels.userCount}`}: </span>
            <span>{`${this.state.userCount}`}</span>
        </div>
        <div className="mb-3">
            <div>{`${this.props.labels.innerProfit}`}: </div>
            {innerProfit}
        </div>
        <div>
            <span>{`${this.props.labels.activeDeposits}`}: </span>
            <span>{this.state.activeDeposit.count}</span>
        </div>
        <div className="mb-2">
            <span>{`${this.props.labels.acceptedDeposits}`}: </span>
            <span>{this.state.acceptedDeposit.count}</span>
        </div>
        <div>
            <div>
                <span>{`${this.props.labels.deposit.income.realTotal}`}</span>
                <span> / </span>
                <span>{`${this.props.labels.deposit.income.feeTotal}`}</span>
                <span>:</span>
            </div>
            {Object.keys(this.state.deposit.income.real).map(k => {
                let i = 0, f = 0
                if(this.state.deposit.income.real.hasOwnProperty(k))
                    i = this.state.deposit.income.real[k] / 100
                if(this.state.deposit.income.fee.hasOwnProperty(k))
                    f = this.state.deposit.income.fee[k] / 100
                return(<div className={`${k.toLowerCase()}`}>
                    <span>{k}</span>
                    <span>: </span>
                    <span>+{i.toFixed(2)}</span>
                    <span> / </span>
                    <span>+{f.toFixed(2)}</span>
                </div>)})
            }
        </div>
        </React.Fragment>)
    }
}