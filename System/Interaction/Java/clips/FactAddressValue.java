package clips;

public class FactAddressValue extends PrimitiveValue
{
	private Environment owner;

	/*********************/
	/* FactAddressValue: */
	/*********************/
	public FactAddressValue(
			long value,
			Environment env)
	{
		super(new Long(value));

		owner = env;
	}

	/*******************/
	/* getEnvironment: */
	/*******************/
	public Environment getEnvironment()
	{ return owner; }

	/*******************/
	/* getFactAddress: */
	/*******************/     
	public long getFactAddress()
	{ return ((Long) getValue()).longValue(); }

	/****************/
	/* getFactSlot: */
	/****************/     
	public PrimitiveValue getFactSlot(
			String slotName) throws Exception
	{ return Environment.getFactSlot(this,slotName); }

	/*****************/
	/* getFactIndex: */
	/*****************/     
	public long getFactIndex()
	{ return Environment.factIndex(this); }

	/***********/
	/* retain: */
	/***********/
	public void retain()
	{
		owner.incrementFactCount(this);
	}

	/************/
	/* release: */
	/************/
	public void release()
	{
		owner.decrementFactCount(this);
	}

	/*************/
	/* toString: */
	/*************/
	public String toString()
	{        
		return "<Fact-" + getFactIndex() + ">";
	}
}
