const async = require('async');

const printAsync = (s, cb) => {
    var delay = Math.floor((Math.random() * 1000) + 500);
    setTimeout(function () {
        console.log(s);
        if (cb) cb();
    }, delay);
};

const task1 = cb => printAsync("1", cb);
const task2 = cb => printAsync("2", cb);
const task3 = cb => printAsync("3", cb);

const loop = n => {
    const tasks = [].concat(...Array(n).fill([task1, task2, task3]));
    async.waterfall(tasks, (err, result) => console.log('done!'));
};

loop(3);