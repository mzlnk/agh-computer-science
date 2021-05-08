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

Fork.prototype.acquire = function (id, cb) {
    const attempt = (i, sum) => {
        console.log(`[FORK-${this.id}][P-${id}] acquire attempt: ${i + 1}`);

        if (this.state === AVAILABLE) {
            console.log(`[FORK-${this.id}][P-${id}] acquired`);
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

Fork.prototype.release = function (id) {
    console.log(`[FORK-${this.id}][P-${id}] released`);
    this.state = AVAILABLE;
}

var Waiter = function (all) {
    this.all = all;
    this.atTable = 0;
}

Waiter.prototype.acquire = function (id, cb) {
    const attempt = i => {
        console.log(`[TABLE] [P-${id}] acquire attempt: ${i + 1}`);

        if(this.atTable < this.all - 1) {
            this.atTable += 1;
            cb();
        } else {
            setTimeout(() => attempt(i + 1), BEB(i));
        }
    }

    setTimeout(() => attempt(0), 1);
}

Waiter.prototype.release = function () {
    this.atTable -= 1;
}

var Philosopher = function (id, forks, waiter) {
    this.id = id;
    this.forks = forks;
    this.waiter = waiter;
    this.f1 = id % forks.length;
    this.f2 = (id + 1) % forks.length;
    return this;
}

Philosopher.prototype.eat = function (cb) {
    console.log(`Philosopher ${this.id} started eating`);
    setTimeout(cb, 1000);
}

Philosopher.prototype.startConductor = function (count, callback) {
    const forks = this.forks,
        f1 = this.f1,
        f2 = this.f2,
        id = this.id,
        waiter = this.waiter;

    const acquireTable = cb => waiter.acquire(id, cb);
    const acquireLeftFork = cb => forks[f1].acquire(id, cb);
    const acquireRightFork = cb => forks[f2].acquire(id, cb);
    const eat = cb => this.eat(cb);

    const tasks = [acquireTable, acquireLeftFork, acquireRightFork, eat];

    const attempt = i => {
        if (i < count) {
            async.waterfall(
                tasks,
                (err, result) => {
                    console.log(`Philosopher ${this.id} finished eating`);
                    forks[f1].release(id);
                    forks[f2].release(id);
                    waiter.release();
                    attempt(i + 1);
                }
            );
        } else {
            callback();
        }
    }

    attempt(0);
}

// ***********************************************************

const analyze = N => {
    allTimeWaiting = 0;

    const count = 3;
    const forks = [];
    const philosophers = []

    for (let i = 0; i < N; i++) {
        forks.push(new Fork(i));
    }

    const waiter = new Waiter(N);

    for (let i = 0; i < N; i++) {
        philosophers.push(new Philosopher(i, forks, waiter));
    }

    const tasks = [];
    for (let i = 0; i < N; i++) {
        tasks.push(cb => philosophers[i].startConductor(count, cb))
    }

    parallel(tasks, () => console.log(`avg time waiting for N = ${N}: ${allTimeWaiting / (N * count)} ms`));
}

// ***********************************************************

analyze(11);

/**
 * N = 5:   5133ms
 * N = 6:   10055ms
 * N = 7:   5653ms
 * N = 8:   26371ms
 * N = 9:   30464ms
 * N = 10:  28394ms
 * N = 11:  405ms
 * N = 12:  350ms
 * N = 13:  260ms
 * N = 14:  330ms
 * N = 15:  148ms
 * N = 16:  156ms
 * N = 17:  194ms
 * N = 18:  154ms
 * N = 19:  246ms
 * N = 20:  206ms
 */

