const async = require('async');

const AVAILABLE = 0;
const ACQUIRED = 1;

const BEB = i => {
    return Math.floor(Math.random() * (2 ** i));
}

var Fork = function(id) {
    this.id = id;
    this.state = AVAILABLE;
    return this;
}

Fork.prototype.acquire = function(id, cb) {
    // zaimplementuj funkcje acquire, tak by korzystala z algorytmu BEB
    // (http://pl.wikipedia.org/wiki/Binary_Exponential_Backoff), tzn:
    // 1. przed pierwsza proba podniesienia widelca Filozof odczekuje 1ms
    // 2. gdy proba jest nieudana, zwieksza czas oczekiwania dwukrotnie
    //    i ponawia probe itd.

    const attempt = i => {
        console.log(`[FORK-${this.id}][P-${id}] acquire attempt: ${i + 1}`);

        if(this.state === AVAILABLE) {
            console.log(`[FORK-${this.id}] acquired`);

            this.state = ACQUIRED;
            cb();
        } else {
            setTimeout(() => attempt(i + 1), BEB(i));
        }
    }

    setTimeout(() => attempt(0), 1);
}

Fork.prototype.release = function() {
    console.log(`[FORK-${this.id}] released`);
    this.state = AVAILABLE;
}

var Philosopher = function(id, forks) {
    this.id = id;
    this.forks = forks;
    this.f1 = id % forks.length;
    this.f2 = (id+1) % forks.length;
    return this;
}

Philosopher.prototype.eat = function (cb) {
    console.log(`Philosopher ${this.id} started eating`);
    setTimeout(cb, 1000);
}

Philosopher.prototype.startNaive = function(count) {
    const forks = this.forks,
        f1 = this.f1,
        f2 = this.f2,
        id = this.id;

    const acquireLeftFork = cb => forks[f1].acquire(id, cb);
    const acquireRightFork = cb => forks[f2].acquire(id, cb);
    const eat = cb => this.eat(cb);

    const tasks = [acquireLeftFork, acquireRightFork, eat];

    const attempt = i => {
        if(i < count) {

            async.waterfall(
                tasks,
                (err, result) => {
                    console.log(`Philosopher ${this.id} finished eating`);
                    forks[f1].release();
                    forks[f2].release();
                    attempt(i + 1);
                }
            );
        }
    }
    attempt(0);
}

const N = 5;
const forks = [];
const philosophers = []

for (let i = 0; i < N; i++) {
    forks.push(new Fork(i));
}

for (let i = 0; i < N; i++) {
    philosophers.push(new Philosopher(i, forks));
}

for (let i = 0; i < N; i++) {
    philosophers[i].startNaive(3);
}