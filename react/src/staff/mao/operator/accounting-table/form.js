import React from 'react'

import Autocomplete from 'react-autocomplete'


export default class Form extends React.Component {
    constructor (props) {
        super(props)
        this.initialState = {
            step: 0,
            currency: 'rur',
            amount: '',
            ratio: '',
            person: ''
        }
        this.state = this.initialState
        this.stepBack = this.stepBack.bind(this)
        this.personStep = this.personStep.bind(this)
    }
    amountStep (currency) {
        const self = this
        return (e) => {
            self.setState({ currency: currency, step: 1 })
            e.preventDefault()
        }
    }
    personStep (e) {
        const { currency, amount, ratio } = this.state
        const amountValue = parseFloat(amount)
        const ratioValue = parseFloat(ratio)
        let c1 = !isNaN(amountValue)
        let c2 = amountValue > 0
        let c3 = !isNaN(ratioValue)
        let c4 = ratioValue > 0
        let conditionMet = c1 && c2
        if (currency === 'rur') {
            conditionMet = conditionMet && c3 && c4
        }
        if (conditionMet) {
            this.setState({ step: 2 })
        } else {
            console.warn(conditionMet)
        }
        e.preventDefault()
    }
    submitStep () {
        if (this.state.person != '') {
            this.setState({ step: 3 })
        }
    }
    stepBack (e) {
        this.setState(s => ({ step: s.step - 1  }))
        e.preventDefault()
    }
    updateAmount (e) {
        this.setState({ amount: e.target.value })
    }
    updateRatio (e) {
        this.setState({ ratio: e.target.value })
    }
    updatePerson (p) {
        this.setState({ person: p })
    }
    resetForm () {
        this.setState(this.initialState)
    }
    submitForm () {
        const { currency, amount, ratio, person } = this.state
        const a = this.props.operation == 'd' ? amount : -amount
        this.props.onSubmit({
            currency: currency,
            amount: a,
            ratio: ratio,
            person: person
        })
        this.resetForm()
    }
    dismissForm () {
        this.resetForm()
        this.props.onDismiss()
    }
    render () {
        const { step, currency, person, amount, ratio } = this.state
        const { operation } = this.props
        const amountValue = parseFloat(amount)
        const ratioValue = parseFloat(ratio)
        return (<React.Fragment>
            {step == 0 && <React.Fragment>
                <div className="row">
                    <div className="col">Выберите валюту</div>
                </div>
                <div className="row mb-2">
                    <div className="col">
                        <ul className="nav nav-pills">
                            <li className="nav-item mr-2">
                                <a className="nav-link active" href="#" onClick={this.amountStep('rur')}>₽</a>
                            </li>
                            <li className="nav-item">
                                <a className="nav-link active" href="#" onClick={this.amountStep('pzm')}>PZM</a>
                            </li>
                        </ul>
                    </div>
                </div>
                <div className="row">
                    <div className="col">
                        <button
                            className="btn btn-outline-secondary"
                            onClick={_ => this.dismissForm()}>Отмена</button>
                    </div>
                </div>
            </React.Fragment>}
            {step == 1 && <React.Fragment>
                <div className="form-group row my-1">
                    <label htmlFor="amount-in">Сумма</label>
                    <input
                        id="amount-in"
                        className="form-control"
                        placeholder="0.00"
                        value={this.state.amount}
                        onChange={e => this.updateAmount(e)}/>
                </div>
                {currency == 'rur' && <div className="form-group row my-1">
                    <label htmlFor="ratio-in">Курс</label>
                    <input
                        id="ratio-in"
                        className="form-control"
                        placeholder="0.00"
                        value={this.state.ratio}
                        onChange={e => this.updateRatio(e)}/>
                </div>}
                <div className="row my-2">
                    <button className="btn btn-outline-secondary" onClick={this.stepBack}>Назад</button>
                    <button className="btn ml-2" onClick={this.personStep}>Далее</button>
                </div>
            </React.Fragment>}
            {step == 2 && <React.Fragment>
                <div className="row my-2">
                    <Autocomplete
                        items={this.props.persons}
                        inputProps={{
                            id: 'person-in',
                            className: 'form-control'
                        }}
                        renderItem={(item) => <div>{item}</div>}
                        getItemValue={item => item}
                        value={person}
                        onChange={(e) => this.updatePerson(e.target.value)}
                        onSelect={(val) => this.updatePerson(val)}
                        shouldItemRender={(item, val) => item.toLowerCase().indexOf(val.toLocaleLowerCase()) !== -1}
                        renderInput={props => <div className="form-group">
                            <label htmlFor="person-in">{`${operation == 'd'? 'Кому' : 'Кто'}`}</label>
                            <input {... props} />
                        </div>}
                        />
                </div>
                <div className="row my-2">
                    <button className="btn btn-outline-secondary" onClick={this.stepBack}>Назад</button>
                    <button className="btn ml-2" onClick={_ => this.submitStep()}>Далее</button>
                </div>
            </React.Fragment>}
            {step == 3 && <React.Fragment>
                <div className="row my-2">
                    <div className="col">
                        <p className="lead">Подтвердите операцию {`${operation == 'd'? 'пополнения' : 'вывода'}`}</p>
                        <dl>
                            <dt>Пользователь</dt>
                            <dd>{person}</dd>
                            <dt>Сумма</dt>
                            <dd>
                                {`${operation == 'd'? '+' : '-'}`}
                                {amountValue}
                                {`${currency === 'rur'? '₽' : 'PZM'}`}</dd>
                            {currency === 'rur' && <React.Fragment>
                                <dt>Курс</dt>
                                <dd>{ratioValue}</dd>
                            </React.Fragment>}

                        </dl>
                    </div>
                </div>
                <div className="row my-2">
                    <button className="btn btn-outline-secondary" onClick={this.stepBack}>Назад</button>
                    <button className="btn ml-2" onClick={_ => this.submitForm()}>Применить</button>
                </div>
            </React.Fragment>}
        </React.Fragment>)
    }
}
