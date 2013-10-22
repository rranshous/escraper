defmodule Escraper.WorkQueue do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :workqueue }, __MODULE__, [], [])
  end

  def init(queue) do
    { :ok, HashSet.new(queue) }
  end

  def handle_cast({ :add, page }, queue) do
    IO.puts "work queue adding: #{page.url}"
    :gen_server.cast(:pagescraper, :work_added)
    { :noreply, HashSet.put(queue, page) }
  end

  def handle_call(:pop, _from, queue) do
    case pop_hashset(queue) do
      { :empty , _ } -> { :reply, { :error, :empty }, queue }
      { h, queue } -> { :reply, { :ok, h }, queue }
    end
  end

  def handle_call({ :in_queue?, url }, _from, queue) do
    case Enum.find(queue, fn(p) -> p.url == url end) do
      nil -> { :reply, false, queue }
      _   -> { :reply, true, queue }
    end
  end

  def pop_hashset(set) do
    case HashSet.size(set) do
      0 -> 
        { :empty, set }
      l ->
        { Enum.first(set), HashSet.delete(set, Enum.first(set)) }
    end
  end

end
