# Parallel programming in Elixir

![Elixir logo](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT2Kncd_AWN2DfPoSvv6sQieZtLlKG4YeLu5FuJn-8g__fZt5uX)


##Introduction

Elixir is a functional programming language built on top of the Erlang VM.
It's supposed to be great for developing highly concurrent, distributed and fault tolerant applications. Since we are masters student with emphasis on distributed systems we were very excited to have some fun with Elixir. 
This will be a short tutorial on how to do parallel programming in Elixir. We will take a sequential merge sort algorithm and make it run in parallel in hopes that it will run faster. 

##Mergesort
For those who are not familiar with the merge sort algorithm, it´s an algorithm that sorts a list of integers by splitting up the list in half, then performs merge sort on both of the sub-lists. Once the lists have been split up until they only contain a single element. Then they are merged back together in sorted order. A visual representation of this can be seen in the following gif.

![Merge sort gif](https://upload.wikimedia.org/wikipedia/commons/c/cc/Merge-sort-example-300px.gif)

##Sequential Mergesort
Here we have an implementation of a sequential mergesort. It consists of two functions merge and seq\_merge\_sort. The merge function is used to merge two ordered lists, it will be used in all of the different merge sort implementations in this tutorial. 
The seq\_merge\_sort function takes care of splitting up the lists, then recursively calling seq_merge_sort on both of the lists before merging them back together.

```
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



##Naive parallel mergesort using tasks
There are few different ways in Elixir to run things in parallel. 
Spawn
Tasks

```
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

```
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

```
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


##Parallel mergesort with granularity using tasks
```
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






