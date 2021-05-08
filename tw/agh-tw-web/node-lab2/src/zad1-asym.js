const async = require('async');

// ***********************************************************

var allTimeWaiting = 0;

// ***********************************************************

const AVAILABLE = 0;
const ACQUIRED = 1;

const BEB = i => {
    return Math.floor(Math.random() * (2 ** i));
}

const parallel = (tasks, callback) => {
    let i = tasks.length;

    tasks.forEach(func => func(() => {
        i--;
        if(i === 0) callback();
    }))
}

// ***********************************************************

var Fork = function (id) {
    this.id = id;
    this.state = AVAILABLE;
    return this;
}

Fork.prototype.acquire = function (cb) {
    const attempt = (i, sum) => {
        console.log(`[FORK-${this.id}] acquire attempt: ${i + 1}`);

        if (this.state === AVAILABLE) {
            console.log(`[FORK-${this.id}] acquired`);
            allTimeWaiting += sum;

            this.state = ACQUIRED;
            cb();
        } else {
            const timeout = BEB(i);
            setTimeout(() => attempt(i + 1, sum + timeout), timeout);
        }
    }

    setTimeout(() => attempt(0, 0), 1);
}

Fork.prototype.release = function () {
    console.log(`[FORK-${this.id}] released`);
    this.state = AVAILABLE;
}

var Philosopher = function (id, forks) {
    this.id = id;
    this.forks = forks;
    this.f1 = id % forks.length;
    this.f2 = (id + 1) % forks.length;
    return this;
}

Philosopher.prototype.eat = function (cb) {
    console.log(`Philosopher ${this.id} started eating`);
    setTimeout(cb, 1000);
}

Philosopher.prototype.startAsym = function (count, callback) {
    const forks = this.forks,
        f1 = this.f1,
        f2 = this.f2,
        id = this.id;

    const acquireLeftFork = cb => forks[f1].acquire(cb);
    const acquireRightFork = cb => forks[f2].acquire(cb);
    const eat = cb => this.eat(cb);

    const tasks = id % 2 === 0 ? [acquireLeftFork, acquireRightFork, eat] : [acquireRightFork, acquireLeftFork, eat];

    const attempt = i => {
        if (i < count) {
            async.waterfall(
                tasks,
                (err, result) => {
                    console.log(`Philosopher ${this.id} finished eating`);
                    forks[f1].release();
                    forks[f2].release();
                    attempt(i + 1);
                }
            );
        } else {
            callback();
        }
    }

    setTimeout(() => attempt(0), 1);
}

// ***********************************************************

const analyze = N => {
    const count = 3;
    const forks = [];
    const philosophers = []

    for (let i = 0; i < N; i++) {
        forks.push(new Fork(i));
    }

    for (let i = 0; i < N; i++) {
        philosophers.push(new Philosopher(i, forks));
    }

    const tasks = [];
    for (let i = 0; i < N; i++) {
        tasks.push(cb => philosophers[i].startAsym(count, cb))
    }

    parallel(tasks, () => console.log(`avg time waiting for N = ${N}: ${allTimeWaiting / (N * count)} ms`));
}

// ***********************************************************

analyze(20);

/**
 * N = 5:   1019ms
 * N = 6:   1633ms
 * N = 7:   1609ms
 * N = 8:   1295ms
 * N = 9:   926ms
 * N = 10:  1640ms
 * N = 11:  1043ms
 * N = 12:  1262ms
 * N = 13:  1160ms
 * N = 14:  1394ms
 * N = 15:  1149ms
 * N = 16:  1301ms
 * N = 17:  1134ms
 * N = 18:  1197ms
 * N = 19:  1357ms
 * N = 20:  963ms
 */
