# Parallel programming in Elixir

![Elixir logo](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT2Kncd_AWN2DfPoSvv6sQieZtLlKG4YeLu5FuJn-8g__fZt5uX)


##Introduction
Elixir is a functional programming language built on top of the Erlang VM.
It's supposed to be great for developing highly concurrent, distributed and fault tolerant applications. We are masters student with emphasis on distributed systems so we were very excited to have some fun with Elixir. We decided to start out with something simple, doing parallel mergesort in Elixir. Doing the same in Erlang had shown that it was possible to get a very good speedup on mergesort. As such, we expected to see a similar speedup in Elixir, since both languages run on the same virtual machine. What we got instead, however, was quite surprising. 


##Mergesort
For those who are not familiar with the merge sort algorithm, it´s an algorithm that sorts a list of integers by splitting up the list in half, then performs merge sort on both of the sub-lists. Once the lists have been split up until they only contain a single element. Then they are merged back together in sorted order. A visual representation of this can be seen in the following gif.

![Merge sort gif](https://upload.wikimedia.org/wikipedia/commons/c/cc/Merge-sort-example-300px.gif)

##Sequential Mergesort
Here we have an implementation of a sequential mergesort. It consists of two functions merge and seq\_merge\_sort. The merge function is used to merge two ordered lists, it will be used in all of the different merge sort implementations in this tutorial. 
The seq\_merge\_sort function takes care of splitting up the lists, then recursively calling seq_merge_sort on both of the lists before merging them back together.

```elixir
  1 defmodule SeqMergeSort do
  2     def merge([], y),           do: y
  3     def merge(x, []),           do: x
  4     def merge([x|xs], [y|ys])   do
  5         if x < y do
  6             [x|merge(xs, [y|ys])]
  7         else
  8             [y|merge(ys, [x|xs])]
  9         end
 10     end
 11 
 12     def seq_merge_sort([]), do: []
 13     def seq_merge_sort(x) do
 14         len = length(x)
 15         if len > 1 do
 16             {l1, l2} = Enum.split(x, round(len/2))
 17             merge(seq_merge_sort(l1), seq_merge_sort(l2))
 18         else
 19             x
 20         end
 21     end
 22 end
```

##Running work in parallel in Elixir 
There are few different ways in Elixir to run things in parallel. We will be looking at the most simplest ways to do it, which are spawn_link and tasks.

###Spawn link
Spawn link in Elixir works just like the Erlang spawn link function, it creates a new background process that can perform some work. Once the work is done it can be passed back as a message to the parent process. 

```elixir
1 pid = spawn(Spawn2, :greet, [])
2 send pid, {self, "Hello World!"}
3 
4 receive do
5 	{sender, msg} ->
6 	  IO.puts message
7 end
```
In the example above it can be seen in line 1 how a new process is spawned. Line two demonstrates how to do message passing between processes and lines 4-7 show how to receive a message.

###Tasks
Elixir tasks is simply an abstraction of the spawn function in Elixir. It allows you to add parallelism to your program in a very simple way.
We can run the following code 

```elixir
1 worker = Task.async(func) 
2 results = Task.await(worker)
```

and this will result in a new process being created in the background which will run the function func that we pass into it. The result of that will be the id/pid of the process that got created, which we can then pass into Task.await() function that returns the results when the computation is finished.

##Naive parallel mergesort using tasks
We created this parallel version of merge sort using tasks. It is almost exactly the same as the sequential version except that we use tasks to spawn a new process to perform the computations.

```elixir
  1 defmodule NaiveParMergeSort do
  2     alias SeqMergeSort, as: Merge
  3 
  4     def naive_par_merge_sort([]), do: []
  5     def naive_par_merge_sort(x) do
  6         len = length(x)
  7         if len > 1 do
  8             {l1, l2} = Enum.split(x, round(len/2))
  9             Merge.merge(
 10                 Task.await(Task.async(NaiveParMergeSort, :naive_par_merge_sort, [l1])),                          
 11                 Task.await(Task.async(NaiveParMergeSort, :naive_par_merge_sort, [l2])))
 12         else
 13             x
 14         end
 15     end
 16 end
```





##Benchmarking and test data
Now in order to see if we are actually getting some speedup by running things in parallel we will have to benchmark both of the implementations. We created the Helpers module that contains two functions random\_list and benchmark. random\_list takes takes care of generating a random list of integers as test data, we pass into the function an upper bound of an interval that the numbers in the list should be within and n which specifies the size of the list.
benchmark takes care of running the benchmark n number of times and returns the average run time of those n runs.

```elixir
  1 defmodule Helpers do
  2     def random_list(interval, n) do
  3         Enum.map(1..n, fn _ -> :rand.uniform(interval) end)
  4     end
  5 
  6     def benchmark(n, module, fun, fun_input) do
  7         runs = for i <- 0..n, i > 0, do:
  8             :timer.tc(module, fun, fun_input)
  9             |> elem(0)
 10         Enum.sum(runs) / length(runs)
 11     end
 12 end
```

Now let's run the benchmark in iex.

```bash
➜  erlang-tutorial git:(master) ✗ iex
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Interactive Elixir (1.6.4) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> c("helpers.exs") 
[Helpers]
iex(2)> c("seq_msort.exs") 
[SeqMergeSort]
iex(3)> c("naive_par_msort.exs")
[NaiveParMergeSort]
iex(4)> test_data = Helpers.random_list(1000000, 200000)
[998312, 637926, 916067, 762978, 108602, 997472, 108920, 665561, 524107, 240520,
 744708, 598909, 990665, 74648, 106934, 16275, 474055, 311630, 501419, 101909,
 365258, 621625, 979912, 333421, 89826, 453457, 106432, 322154, 706833, 67345,
 990594, 819708, 215055, 700666, 252557, 554486, 554162, 885134, 747419, 891437,
 234401, 473644, 141608, 612533, 570701, 49984, 653690, 681996, 610417, 222045,
 ...]
iex(5)> Helpers.benchmark(10, SeqMergeSort, :seq_merge_sort, [test_data])
179309.4
iex(6)> Helpers.benchmark(10, NaiveParMergeSort, :naive_par_merge_sort, [test_data])
2442086.5
```

By looking at the results we can see that something is wrong with our parallel implementation, since it takes much longer time than the sequential version.
For those who have done some parallel programming before this is probably not suprising. Even though the overhead of spawning new processes in very low in the Erlang virtual machine it still does not make sense to spawn a process for every single sub list that needs to be sorted. We will fix this by adding granularity.


##Parallel mergesort with granularity using tasks



```elixir
  1 defmodule GranParMergeSort do
  2     alias SeqMergeSort, as: Merge
  3 
  4     def gran_par_merge_sort([], _), do: []
  5     def gran_par_merge_sort(x, depth) do
  6         len = length(x)
  7         if len > 1 do
  8             {l1, l2} = Enum.split(x, round(len/2))
  9             if depth > 0 do
 10                 Merge.merge(
 11                     Task.await(Task.async(GranParMergeSort, :gran_par_merge_sort, [l1, depth - 1])),
 12                     Task.await(Task.async(GranParMergeSort, :gran_par_merge_sort, [l2, depth - 1])))
 13             else
 14                     Merge.merge(gran_par_merge_sort(l1, 0), gran_par_merge_sort(l2, 0))
 15             end
 16         else
 17             x
 18         end
 19     end
 20 end
```


##Parallel mergesort with granularity using spawn link
```elixir
  1 defmodule ErlPsort do
  2     alias SeqMergeSort, as: Merge
  3 
  4     def start(x,depth) do
  5       erl_psort(self(),x,depth)
  6       receive do
  7         x -> x
  8       end
  9     end
 10 
 11     def erl_psort(pid,[], _), do: send pid, []
 12     def erl_psort(pid, x, depth) do
 13         len = length(x)
 14         if len > 1 do
 15             {l1, l2} = Enum.split(x, round(len/2))
 16             if depth > 0 do
 17                 spawn_link(ErlPsort, :erl_psort, [self(),l1,depth-1])
 18                 spawn_link(ErlPsort, :erl_psort, [self(),l2,depth-1])
 19                 receive do
 20                   x -> h1 = x
 21                 end
 22                 receive do
 23                   y -> send pid, Merge.merge(y,h1)
 24                 end
 25             else
 26                 send pid, Merge.merge(Merge.seq_merge_sort(l1), Merge.seq_merge_sort(l2))
 27             end
 28         else
 29             send pid, x
 30         end
 31     end
 32 end
```



##Our hypothesis of the problem
We didn’t have time to investigate further on why Tasks are slowing things down as much as they do in our case. We think it has something to do with the split lists getting copied into every new process created in the Tasks. We build this hypothesis on the fact that the benchmarks  gets worse the more we increased the depth. Though some overhead is expected when spawning new processes (that is why we did a version using granularity in the first place) the amount we spawned should have nowhere near the effect it had in the results, given how light-weight processes are in the Erlang VM.  

##In conclusion
It turns out Tasks are quite dangerous to play around with. Even though they are marketed as simple abstractions on top of spawn_link it is very easy to use them in such a way that all of the potential speedup of making your code parallel disappears. 

The final lesson from all this is probably to always benchmark your code properly. We would never have suspected our implementation to be faulty unless we saw the results of running it. 


##References

Mergesort gif: https://upload.wikimedia.org/wikipedia/commons/c/cc/Merge-sort-example-300px.gif

Elixir logo: https://encrypted-tbn0.gstatic.com/images?
q=tbn:ANd9GcT2Kncd_AWN2DfPoSvv6sQieZtLlKG4YeLu5FuJn-8g__fZt5uX

Elixir homepage: https://elixir-lang.org/

Book by Dave Thomas: Programming Elixir