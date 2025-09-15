#[derive(Debug)]
pub struct WalkerConfig {
    pub depth_step: usize,
    pub depth_limit: usize,
    pub answer_limit: usize,
    pub print_iter: bool,
    pub print_stat: bool,
    pub answer_pause: bool,
}

impl WalkerConfig {
    pub fn new() -> WalkerConfig {
        WalkerConfig {
            depth_step: 5,
            depth_limit: 100,
            answer_limit: usize::MAX,
            print_iter: true,
            print_stat: true,
            answer_pause: false,
        }
    }
}

#[derive(Debug)]
pub struct WalkerStat {
    pub step_cnt: usize,
    pub step_cnt_la: usize,
    pub total_step: usize,
    pub acc_total_step: usize,
}

impl WalkerStat {
    pub fn new() -> WalkerStat {
        WalkerStat {
            step_cnt: 0,
            step_cnt_la: 0,
            total_step: 0,
            acc_total_step: 0,
        }
    }

    pub fn reset(&mut self) {
        self.step_cnt = 0;
        self.step_cnt_la = 0;
        self.total_step = 0;
    }

    pub fn step(&mut self) {
        self.step_cnt += 1;
        self.total_step += 1;
        self.acc_total_step += 1;
    }

    pub fn step_la(&mut self) {
        self.step_cnt_la += 1;
        self.total_step += 1;
        self.acc_total_step += 1;
    }

    pub fn print_stat(&self) -> String {
        format!(
            "[STAT]: step = {}, step_la = {}(ratio {}), total = {}, acc_total = {} ",
            self.step_cnt,
            self.step_cnt_la,
            (self.step_cnt_la as f32) / (self.step_cnt as f32 + 0.001),
            self.total_step,
            self.acc_total_step,
        )
    }
}
